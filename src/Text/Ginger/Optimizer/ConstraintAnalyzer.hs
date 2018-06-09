{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RankNTypes #-}

module Text.Ginger.Optimizer.ConstraintAnalyzer
where

import Text.Ginger.AST
import Text.Ginger.GVal
import Text.Ginger.Run
import Data.Semigroup
import Data.Monoid hiding ( (<>) )
import Control.Monad.Identity
import Data.Default
import Control.Monad.Writer (Writer, execWriter, tell)
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Text (Text)
import qualified Data.Aeson as JSON
import Data.List
import qualified Data.Text as Text

-- * Representing Evidence

-- | A generalized reference to something that is in scope somewhere, and that
-- we know something about.
data ScopeRef
  = CurrentItem -- ^ Refer to the scope itself
  | NamedItem VarName ScopeRef -- ^ A single child, accessed by name (@foo.bar@ or @foo["bar"]@)
  | NumberedItem Int ScopeRef -- ^ A single child, accessed by index (@foo[1]@)
  deriving (Eq, Ord, Show)

scopeAppend :: ScopeRef -> ScopeRef -> ScopeRef
scopeAppend CurrentItem a = a
scopeAppend a CurrentItem = a
scopeAppend a (NamedItem n b) = NamedItem n (scopeAppend a b)
scopeAppend a (NumberedItem n b) = NumberedItem n (scopeAppend a b)

instance Semigroup ScopeRef where
  (<>) = scopeAppend

instance Monoid ScopeRef where
  mempty = CurrentItem
  mappend = scopeAppend

data Constraint
  = Constant -- ^ This value may not be known at compile time, but we do know that
             -- its value does not change anywhere within its scope.
  | Static -- ^ This value is known to be static, i.e. fully determined at
           -- compile time.
  | PureFn -- ^ This value represents a pure function, i.e. applying it has no
           -- side effects.
  | KnownValue (GVal Identity) -- ^ We actually know the value at compile time.
  deriving (Show)

instance Ord Constraint where
  compare = compareConstraint

compareConstraint Constant Constant = EQ
compareConstraint Constant _ = LT
compareConstraint Static Constant = GT
compareConstraint Static Static = EQ
compareConstraint Static _ = LT
compareConstraint PureFn Constant = GT
compareConstraint PureFn Static = GT
compareConstraint PureFn PureFn = EQ
compareConstraint PureFn _ = LT
compareConstraint (KnownValue a) (KnownValue b) = compare (asText a) (asText b)
compareConstraint (KnownValue _) _ = GT

instance Eq Constraint where
  a == b = compare a b == EQ

type Constraints = Set Constraint

newtype Evidence = Evidence { unEvidence :: Map ScopeRef (Set Constraint) }
  deriving (Show, Eq, Ord)

instance Semigroup Evidence where
  (<>) = appendEvidence

instance Monoid Evidence where
  mempty = noEvidence
  mappend = (<>)

-- * Pretty-printing Evidence

ppConstraint :: Constraint -> String
ppConstraint (KnownValue v) = show . asText $ v
ppConstraint c = show c

ppConstraints :: Constraints -> String
ppConstraints constraints
  | Set.null constraints = "Unconstrained"
  | otherwise = unwords . map ppConstraint . Set.toAscList $ constraints

ppRef :: ScopeRef -> String
ppRef CurrentItem = "."
ppRef (NamedItem n CurrentItem) = Text.unpack n
ppRef (NamedItem n parent) = ppRef parent ++ "[" ++ show n ++ "]"
ppRef (NumberedItem i CurrentItem) = "[" ++ show i ++ "]"
ppRef (NumberedItem i parent) = ppRef parent ++ "[" ++ show i ++ "]"

ppEvidence' :: Evidence -> [String]
ppEvidence' ev =
  [ ppRef ref ++ " is " ++ ppConstraints constraints
  | (ref, constraints) <- Map.toAscList (unEvidence ev)
  ]

ppEvidence :: Evidence -> String
ppEvidence = unlines . ppEvidence'

-- * Manipulating Evidence

appendEvidence :: Evidence -> Evidence -> Evidence
appendEvidence (Evidence a) (Evidence b) =
  Evidence $ Map.unionWith (<>) a b

emptySetToNothing :: Set a -> Maybe (Set a)
emptySetToNothing s
  | Set.null s = Nothing
  | otherwise = Just s

alterConstraints :: (Constraints -> Constraints) -> ScopeRef -> Evidence -> Evidence
alterConstraints f ref =
  Evidence
  . Map.alter
      (emptySetToNothing . f . fromMaybe Set.empty)
      ref
  . unEvidence

addConstraint :: Constraint -> ScopeRef -> Evidence -> Evidence
addConstraint constraint =
  alterConstraints (Set.insert constraint)

removeConstraint :: Constraint -> ScopeRef -> Evidence -> Evidence
removeConstraint constraint =
  alterConstraints (Set.delete constraint)

clearConstraints :: ScopeRef -> Evidence -> Evidence
clearConstraints =
  alterConstraints (const Set.empty)

setConstraints :: Constraints -> ScopeRef -> Evidence -> Evidence
setConstraints constr =
  alterConstraints (const constr)

getConstraints :: ScopeRef -> Evidence -> Constraints
getConstraints ref ev =
  fromMaybe noConstraints . Map.lookup ref . unEvidence $ ev

hasConstraint :: ScopeRef -> Constraint -> Evidence -> Bool
hasConstraint ref constraint ev =
  constraint `Set.member` getConstraints ref ev

-- | The null hypothesis: we know nothing about anything.
noEvidence :: Evidence
noEvidence = Evidence Map.empty

-- | Unconstrained: we know nothing about this name.
noConstraints :: Constraints
noConstraints = Set.empty

-- | Create a singleton evidence from a set of constraints, binding the
-- constraints to the top-level scope ('CurrentItem').
singletonEvidence :: Constraints -> Evidence
singletonEvidence = Evidence . Map.singleton CurrentItem

-- | Port an 'Evidence' by mounting it onto the given 'ScopeRef'.
portEvidence :: ScopeRef -> Evidence -> Evidence
portEvidence parentRef ev =
  Evidence . Map.mapKeys (parentRef <>) . unEvidence $ ev

-- * Inferring Evidence

-- | Infer constraints for a given expression. In the returned pair, the
-- first element tells us the external constraints for the expression, while
-- the second element tells us how the expression affects the containing
-- scope. The reason we have to return them separately is because we need the
-- external constraints for the current expression when analyzing the
-- containing scope, but the expression itself doesn't know what it will be
-- bound to, so we have nowhere in the returned 'Evidence' to put it.
inferExprConstraints :: Evidence -> Expression a -> (Evidence, Evidence)

-- Literals are easy: they are always 'Static' and 'Constant', and they do not
-- modify the environment.
inferExprConstraints ev (StringLiteralE _ s) =
  (singletonEvidence [Static, Constant, KnownValue (toGVal s)], ev)
inferExprConstraints ev (NumberLiteralE _ n) =
  (singletonEvidence [Static, Constant, KnownValue (toGVal n)], ev)
inferExprConstraints ev (BoolLiteralE _ b) =
  (singletonEvidence [Static, Constant, KnownValue (toGVal b)], ev)
inferExprConstraints ev (NullLiteralE _) =
  (singletonEvidence [Static, Constant, KnownValue def], ev)

-- Variable lookups inherit the constraints of the thing they look up.
inferExprConstraints ev (VarE _ varName) =
  let selfEv = singletonEvidence $ getConstraints (NamedItem varName CurrentItem) ev
  in (selfEv, ev)

-- List literals give us two kinds of evidence. The first one is evidence about
-- individual list items, which we simply inherit from the items themselves.
-- The second one is that the list itself inherits all the constraints that
-- hold for all of its members.
inferExprConstraints ev (ListE _ xs) =
  let (childEvidence, ev') =
        inferChildEvidence ev
          (zip [NumberedItem i CurrentItem | i <- [0..]] xs)
      selfEvidence =
        singletonEvidence .
          Map.foldl' Set.intersection [Static, Constant] .
          unEvidence $
          childEvidence
  in (selfEvidence <> childEvidence, ev')

-- Object literals are bit more complicated, because the keys may or may not
-- be known at compile time. Worse yet, keys later in the definition overwrite
-- earlier ones, so in { "foo": "bar", getKey(): "pizza" }, we don't really
-- know anything about the "foo" key after all.
inferExprConstraints ev (ObjectE _ xs) =
  let (childEvidence, ev') =
        inferChildPairEvidence ev xs
      selfEvidence =
        singletonEvidence .
          Map.foldl' Set.intersection [Static, Constant] .
          unEvidence $
          childEvidence
  in (selfEvidence <> childEvidence, ev')

-- By default, we assume that anything goes: any expression can in principle
-- void any and all assumptions we can make about the current scope.
inferExprConstraints ev expr =
  (noEvidence, noEvidence)

inferChildEvidence :: Evidence -> [(ScopeRef, Expression a)] -> (Evidence, Evidence)
inferChildEvidence ev ((ref, expr):exprs) =
  let (myEvidence, ev') = inferExprConstraints ev expr
      (tailEvidence, ev'') = inferChildEvidence ev' exprs
  in (portEvidence ref myEvidence <> tailEvidence, ev'')
inferChildEvidence ev [] =
  (noEvidence, ev)

-- | Given a set of known constraints on an expression, try to figure out the
-- runtime expression value and turn it into a relative 'ScopeRef'. If this is
-- possible, the result will be 'Just' a 'NumberedItem' or 'NamedItem',
-- otherwise it will be 'Nothing'.
refFromKeyConstraints :: Constraints -> Maybe ScopeRef
refFromKeyConstraints constraints =
  go $ Set.toAscList constraints
  where
    go [] = Nothing
    go (KnownValue gval:_) = Just $ NamedItem (asText gval) CurrentItem
    go (_:xs) = go xs
  
inferChildPairEvidence :: Evidence -> [(Expression a, Expression a)] -> (Evidence, Evidence)
inferChildPairEvidence ev ((keyExpr, valExpr):exprs) =
  let (keyEvidence, ev') = inferExprConstraints ev keyExpr
      (valEvidence, ev'') = inferExprConstraints ev' valExpr
      (tailEvidence, ev''') = inferChildPairEvidence ev'' exprs
      -- We can only generate useful relative evidence if we know the key at
      -- compile time.
      myEvidence =
        case refFromKeyConstraints (getConstraints CurrentItem keyEvidence) of
          Nothing ->
            -- We don't know the key yet, so we cannot generate any evidence.
            noEvidence
          Just ref ->
            -- We do know the key, so we generate evidence for it.
            portEvidence ref valEvidence
  in (myEvidence <> tailEvidence, ev''')
inferChildPairEvidence ev [] =
  (noEvidence, ev)

inferStmtConstraints :: Evidence -> Statement a -> Evidence
inferStmtConstraints ev stmt = noEvidence

inferTemplateConstraints :: Evidence -> Template a -> Evidence
inferTemplateConstraints ev tpl = noEvidence
