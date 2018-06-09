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

-- We need this stupid Ord instance because 'GVal' doesn't have a sensible one.
-- It's not an entirely honest instance, really, but for the sake of putting
-- it in a 'Map', it's good enough.

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

addConstraints :: Constraints -> ScopeRef -> Evidence -> Evidence
addConstraints constraints =
  alterConstraints (<> constraints)

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

-- * The 'Infer' Monad

type Infer = State Evidence

runInfer :: Infer a -> Evidence -> (a, Evidence)
runInfer action ev = runState action ev

importEvidence :: ScopeRef -> Evidence -> Infer ()
importEvidence parentRef ev =
  modify (<> portEvidence parentRef ev)

testify :: ScopeRef -> Constraints -> Infer ()
testify ref constr =
  modify (addConstraints constr ref)

testifyHere :: Constraints -> Infer ()
testifyHere = testify CurrentItem

-- * Inferring Evidence

-- | Infer constraints for a given expression.
inferExprConstraints :: Expression a -> Infer Evidence

-- Literals are easy: they are always 'Static' and 'Constant', and they do not
-- modify the environment.
inferExprConstraints (StringLiteralE _ s) =
  pure . singletonEvidence $ [Static, Constant, KnownValue (toGVal s)]
inferExprConstraints (NumberLiteralE _ n) =
  pure . singletonEvidence $ [Static, Constant, KnownValue (toGVal n)]
inferExprConstraints (BoolLiteralE _ b) =
  pure . singletonEvidence $ [Static, Constant, KnownValue (toGVal b)]
inferExprConstraints (NullLiteralE _) =
  pure . singletonEvidence $ [Static, Constant, KnownValue def]

-- Variable lookups inherit the constraints of the thing they look up.
inferExprConstraints (VarE _ varName) =
  singletonEvidence <$> gets (getConstraints (NamedItem varName CurrentItem))

-- List literals give us two kinds of evidence. The first one is evidence about
-- individual list items, which we simply inherit from the items themselves.
-- The second one is that the list itself inherits all the constraints that
-- hold for all of its members.
inferExprConstraints (ListE _ xs) = do
  childEvidence <- inferChildEvidence
                      (zip [NumberedItem i CurrentItem | i <- [0..]] xs)
  let selfEvidence =
        singletonEvidence .
          Map.foldl' Set.intersection [Static, Constant] .
          unEvidence $
          childEvidence
  pure $ selfEvidence <> childEvidence

-- Object literals are bit more complicated, because the keys may or may not
-- be known at compile time. Worse yet, keys later in the definition overwrite
-- earlier ones, so in { "foo": "bar", getKey(): "pizza" }, we don't really
-- know anything about the "foo" key after all.
inferExprConstraints (ObjectE _ xs) = do
  childEvidence <- inferChildPairEvidence xs
  let selfEvidence =
        singletonEvidence .
          Map.foldl' Set.intersection [Static, Constant] .
          unEvidence $
          childEvidence
  pure $ selfEvidence <> childEvidence

-- MemberLookupE a (Expression a) (Expression a) -- ^ foo[bar] (also dot access)
-- CallE a (Expression a) [(Maybe Text, (Expression a))] -- ^ foo(bar=baz, quux)
-- LambdaE a [Text] (Expression a) -- ^ (foo, bar) -> expr
-- TernaryE a (Expression a) (Expression a) (Expression a) -- ^ expr ? expr : expr
-- DoE a (Statement a) -- ^ do { statement; }

-- By default, we assume that anything goes: any expression can in principle
-- void any and all assumptions we can make about the current scope.
inferExprConstraints expr =
  pure noEvidence

inferChildEvidence :: [(ScopeRef, Expression a)] -> Infer Evidence
inferChildEvidence ((ref, expr):exprs) = do
  myEvidence <- inferExprConstraints expr
  tailEvidence <- inferChildEvidence exprs
  pure $ portEvidence ref myEvidence <> tailEvidence
inferChildEvidence [] =
  pure noEvidence

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
  
inferChildPairEvidence :: [(Expression a, Expression a)] -> Infer Evidence
inferChildPairEvidence ((keyExpr, valExpr):exprs) = do
  keyEvidence <- inferExprConstraints keyExpr
  valEvidence <- inferExprConstraints valExpr
  tailEvidence <- inferChildPairEvidence exprs
  -- We can only generate useful relative evidence if we know the key at
  -- compile time.
  let myEvidence =
        case refFromKeyConstraints (getConstraints CurrentItem keyEvidence) of
          Nothing ->
            -- We don't know the key yet, so we cannot generate any evidence.
            noEvidence
          Just ref ->
            -- We do know the key, so we generate evidence for it.
            portEvidence ref valEvidence
  pure $ myEvidence <> tailEvidence
inferChildPairEvidence [] =
  pure noEvidence

inferStmtConstraints :: Statement a -> Infer Evidence
inferStmtConstraints stmt = pure noEvidence

inferTemplateConstraints :: Template a -> Infer Evidence
inferTemplateConstraints tpl = pure noEvidence
