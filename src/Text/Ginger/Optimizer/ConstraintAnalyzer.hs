{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

module Text.Ginger.Optimizer.ConstraintAnalyzer
where

import Text.Ginger.AST
import Text.Ginger.GVal
import Text.Ginger.Run
import Text.Ginger.Parse
import Text.Ginger.Html
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
import Data.Maybe (fromMaybe, catMaybes)
import Control.Applicative
import Data.Text (Text)
import qualified Data.Aeson as JSON
import Data.List
import qualified Data.Text as Text
import Data.Default

-- * Representing Evidence

-- | A generalized reference to something that is in scope somewhere, and that
-- we know something about.
data ScopeRef
  = CurrentItem -- ^ Refer to the scope itself
  | NamedItem VarName ScopeRef -- ^ A single child, accessed by name (@foo.bar@ or @foo["bar"]@)
  | NumberedItem Int ScopeRef -- ^ A single child, accessed by index (@foo[1]@)
  deriving (Eq, Ord, Show)

numberedItem :: Int -> ScopeRef
numberedItem i = NumberedItem i CurrentItem

namedItem :: VarName -> ScopeRef
namedItem n = NamedItem n CurrentItem

makeRefRelativeTo :: ScopeRef -- ^ candidate
                  -> ScopeRef -- ^ parent
                  -> Maybe ScopeRef
-- Making anything relative to the current item is a no-op
makeRefRelativeTo x CurrentItem =
  Just x
makeRefRelativeTo x y
  | x == y = Just CurrentItem
-- If both refs start with the same name reference, pop it off of both and
-- recurse.
makeRefRelativeTo (NamedItem a x) y =
  NamedItem a <$> makeRefRelativeTo x y
-- If both refs start with the same number reference, pop it off of both and
-- recurse.
makeRefRelativeTo (NumberedItem a x) y =
  NumberedItem a <$> makeRefRelativeTo x y
makeRefRelativeTo _ _ =
  Nothing


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
  = Static -- ^ This value is known to be static, i.e. fully determined at
           -- compile time.
  | PureFn -- ^ This value represents a pure function, i.e. applying it has no
           -- side effects.
  | KnownValue (GVal Identity) -- ^ We actually know the value at compile time.
  | KnownOutput (GVal Identity) -- ^ We know what it outputs
  deriving (Show)

-- We need this stupid Ord instance because 'GVal' doesn't have a sensible one.
-- It's not an entirely honest instance, really, but for the sake of putting
-- it in a 'Map', it's good enough.

instance Ord Constraint where
  compare = compareConstraint

compareConstraint Static Static = EQ
compareConstraint Static _ = LT
compareConstraint PureFn Static = GT
compareConstraint PureFn PureFn = EQ
compareConstraint PureFn _ = LT
compareConstraint (KnownValue _) (KnownOutput _) = LT
compareConstraint (KnownValue a) (KnownValue b) = compare (asText a) (asText b)
compareConstraint (KnownValue _) _ = GT
compareConstraint (KnownOutput a) (KnownOutput b) = compare (asText a) (asText b)
compareConstraint (KnownOutput _) _ = GT

instance Eq Constraint where
  a == b = compare a b == EQ

data Constraints =
  Constraints
    { isStatic :: Bool
    , isPure :: Bool
    , knownValue :: Maybe (GVal Identity)
    , knownOutput :: Maybe (GVal Identity)
    }
    deriving (Show)

unconstrained :: Constraints
unconstrained =
  Constraints False False Nothing Nothing

staticConstraints :: Constraints
staticConstraints =
  unconstrained { isStatic = True }

knownValueConstraints :: GVal Identity -> Constraints
knownValueConstraints val =
  knownConstraints val def

knownConstraints :: GVal Identity -> GVal Identity -> Constraints
knownConstraints val output =
  unconstrained
    { isStatic = True
    , knownValue = Just val
    , knownOutput = Just output
    }

appendConstraints :: Constraints -> Constraints -> Constraints
appendConstraints a b =
  Constraints
    { isStatic = isStatic a || isStatic b
    , isPure = isPure a || isPure b
    , knownValue = knownValue b <|> knownValue a
    , knownOutput = knownOutput b <|> knownOutput a
    }

newtype Evidence = Evidence { unEvidence :: Map ScopeRef Constraints }
  deriving (Show)

instance Semigroup Evidence where
  (<>) = appendEvidence

instance Monoid Evidence where
  mempty = noEvidence
  mappend = (<>)

-- * Pretty-printing Evidence

ppConstraints :: Constraints -> String
ppConstraints c =
  intercalate ", " $ catMaybes
    [ if isStatic c then Just "static" else Nothing
    , if isPure c then Just "pure" else Nothing
    , ("outputs: " ++) . show . htmlSource . asHtml <$> knownOutput c
    , ("value: " ++) . show . asText <$> knownValue c
    ]

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
  Evidence $ Map.unionWith appendConstraints a b

emptySetToNothing :: Set a -> Maybe (Set a)
emptySetToNothing s
  | Set.null s = Nothing
  | otherwise = Just s

alterConstraints :: (Constraints -> Constraints) -> ScopeRef -> Evidence -> Evidence
alterConstraints f ref =
  Evidence
  . Map.alter
      (Just . f . fromMaybe unconstrained)
      ref
  . unEvidence

clearConstraints :: ScopeRef -> Evidence -> Evidence
clearConstraints =
  alterConstraints (const unconstrained)

setConstraints :: Constraints -> ScopeRef -> Evidence -> Evidence
setConstraints constr =
  alterConstraints (const constr)

getConstraints :: ScopeRef -> Evidence -> Constraints
getConstraints ref ev =
  fromMaybe unconstrained . Map.lookup ref . unEvidence $ ev

-- | The null hypothesis: we know nothing about anything.
noEvidence :: Evidence
noEvidence = Evidence Map.empty

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

-- | Import an 'Evidence' into the current context below the specified
-- name.
importEvidence :: ScopeRef -- ^ mount point
               -> Evidence -- ^ evidence to import
               -> Infer ()
importEvidence parentRef ev =
  modify (<> portEvidence parentRef ev)

-- * Inferring Evidence
-- ** Expressions

-- | Infer constraints for a given expression.
inferExpr :: Expression a -> Infer Evidence

-- Literals are easy: they are always 'Static', and they do not modify the
-- environment.
inferExpr (StringLiteralE _ s) =
  pure . singletonEvidence $ knownValueConstraints (toGVal s)
inferExpr (NumberLiteralE _ n) =
  pure . singletonEvidence $ knownValueConstraints (toGVal n)
inferExpr (BoolLiteralE _ b) =
  pure . singletonEvidence $ knownValueConstraints (toGVal b)
inferExpr (NullLiteralE _) =
  pure . singletonEvidence $ knownValueConstraints def

-- Variable lookups inherit the constraints of the thing they look up.
inferExpr (VarE _ varName) =
  singletonEvidence <$> gets (getConstraints (NamedItem varName CurrentItem))

-- List literals give us two kinds of evidence. The first one is evidence about
-- individual list items, which we simply inherit from the items themselves.
-- The second one is that the list itself inherits all the constraints that
-- hold for all of its members.
inferExpr (ListE _ xs) = do
  childEvidence <- inferChildEvidence
                      (zip [NumberedItem i CurrentItem | i <- [0..]] xs)
  let selfEvidence =
        singletonEvidence .
          Map.foldl' appendConstraints (knownConstraints def def) .
          unEvidence $
          childEvidence
  pure $ selfEvidence <> childEvidence

-- Object literals are bit more complicated, because the keys may or may not
-- be known at compile time. Worse yet, keys later in the definition overwrite
-- earlier ones, so in { "foo": "bar", getKey(): "pizza" }, we don't really
-- know anything about the "foo" key after all.
inferExpr (ObjectE _ xs) = do
  childEvidence <- inferChildPairEvidence xs
  let selfEvidence =
        singletonEvidence .
          Map.foldl' appendConstraints (knownConstraints def def) .
          unEvidence $
          childEvidence
  pure $ selfEvidence <> childEvidence

inferExpr (MemberLookupE _ parent key) = do
  parentEvidence <- inferExpr parent
  keyEvidence <- inferExpr key
  let constraints = getConstraints CurrentItem keyEvidence
      keyRef = refFromKeyConstraints constraints

  case keyRef of
    Nothing ->
      -- If the key isn't known, there is nothing we can do
      pure noEvidence
    Just ref -> do
      -- We have a key; now we do two things in one pass:
      -- 1. Find the evidence that at or below the ref we just found
      -- 2. Make its key relative to the ref we just found
      let predicate :: (ScopeRef, Constraints) -> Maybe (ScopeRef, Constraints)
          predicate (k, v) = do
            k' <- k `makeRefRelativeTo` ref
            pure (k', v)
      pure
        . Evidence
        . Map.fromList
        . catMaybes
        . map predicate
        . Map.toList
        . unEvidence
        $ parentEvidence

-- CallE a (Expression a) [(Maybe Text, (Expression a))] -- ^ foo(bar=baz, quux)
-- LambdaE a [Text] (Expression a) -- ^ (foo, bar) -> expr
inferExpr (TernaryE _ condE yesE noE) = do
  condEvidence <- inferExpr condE
  case knownValueMaybe (getConstraints CurrentItem condEvidence) of
    Nothing -> do
      -- We don't actually know which of the two branches will run, so we need
      -- to inspect them both, and then combine them using intersection. That
      -- is, we can only retain evidence that applies to both branches.
      -- TODO: actually do the above.
      pure $ singletonEvidence unconstrained
    Just val ->
      case asBoolean val of
        True -> inferExpr yesE
        False -> inferExpr noE

inferExpr (DoE _ stmt) =
  inferStmt stmt

-- By default, we assume that anything goes: any expression can in principle
-- void any and all assumptions we can make about the current scope.
inferExpr expr =
  pure noEvidence

inferChildEvidence :: [(ScopeRef, Expression a)] -> Infer Evidence
inferChildEvidence ((ref, expr):exprs) = do
  myEvidence <- inferExpr expr
  tailEvidence <- inferChildEvidence exprs
  pure $ portEvidence ref myEvidence <> tailEvidence
inferChildEvidence [] =
  pure noEvidence

-- | Given a set of known constraints on an expression, try to figure out the
-- runtime expression value and turn it into a relative 'ScopeRef'. If this is
-- possible, the result will be 'Just' a 'NumberedItem' or 'NamedItem',
-- otherwise it will be 'Nothing'.
refFromKeyConstraints :: Constraints -> Maybe ScopeRef
refFromKeyConstraints constraints = do
  gval <- knownValueMaybe constraints
  fmap (numberedItem . round) (asNumber gval)
    <|> Just (namedItem (asText gval))

knownValueMaybe :: Constraints -> Maybe (GVal Identity)
knownValueMaybe = knownValue
  
inferChildPairEvidence :: [(Expression a, Expression a)] -> Infer Evidence
inferChildPairEvidence ((keyExpr, valExpr):exprs) = do
  keyEvidence <- inferExpr keyExpr
  valEvidence <- inferExpr valExpr
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

-- ** Statements

inferStmt :: Statement a -> Infer Evidence
inferStmt (MultiS _ stmts) = do
  -- Multi statements sequence their children. This means that:
  -- - known output is concatenated
  -- - if any child has unknown output, the output of the MultiS is also
  --   unknown
  -- - the last child determines the return value, ergo the known value of
  --   the whole thing, if any, is the known value of the last child
  -- - purity and staticness are determined in a subtractive fashion
  sequentialEvidence <$> mapM inferStmt stmts

inferStmt (ScopedS _ body) = do
  -- For a scoped statement, we simply remember the original evidence context
  -- and restore it after we're done inferring this statement.
  context <- get
  inferStmt body <* put context

-- IndentS a (Expression a) (Statement a) -- ^ Establish an indented context around the wrapped statement
--
inferStmt (LiteralS _ val) =
  -- Literals are always known
  pure . singletonEvidence $ knownConstraints def (toGVal val)

inferStmt (InterpolationS _ expr) = do
  exprEvidence <- inferExpr expr
  case knownValueMaybe (getConstraints CurrentItem exprEvidence) of
    Nothing -> pure exprEvidence
    Just value ->
      pure $
        alterConstraints
          (\c -> c { knownOutput = Just value })
          CurrentItem
          exprEvidence

inferStmt (ExpressionS _ expr) =
  inferExpr expr
inferStmt (IfS _ condExpr yesStmt noStmt) = do
  condEvidence <- inferExpr condExpr
  case knownValueMaybe (getConstraints CurrentItem condEvidence) of
    Nothing -> do
      -- We don't actually know which of the two branches will run, so we need
      -- to inspect them both, and then combine them using intersection. That
      -- is, we can only retain evidence that applies to both branches.
      -- TODO: actually do the above.
      pure $ singletonEvidence unconstrained
    Just val ->
      case asBoolean val of
        True -> inferStmt yesStmt
        False -> inferStmt noStmt
-- SwitchS a (Expression a) [((Expression a), (Statement a))] (Statement a) -- ^ {% switch expression %}{% case expression %}statement{% endcase %}...{% default %}statement{% enddefault %}{% endswitch %}
-- ForS a (Maybe VarName) VarName (Expression a) (Statement a) -- ^ {% for index, varname in expression %}statement{% endfor %}
inferStmt (SetVarS _ name expr) = do
  exprEvidence <- inferExpr expr
  importEvidence (namedItem name) exprEvidence
  pure . singletonEvidence $ knownValueConstraints def

-- DefMacroS a VarName (Macro a) -- ^ {% macro varname %}statements{% endmacro %}
-- BlockRefS a VarName
-- PreprocessedIncludeS a (Template a) -- ^ {% include "template" %}
inferStmt (NullS _) =
  pure . singletonEvidence $ knownValueConstraints def
-- TryCatchS a (Statement a) [CatchBlock a] (Statement a) -- ^ Try / catch / finally
inferStmt stmt = pure noEvidence

sequentialEvidence :: [Evidence] -> Evidence
sequentialEvidence =
  Evidence
    . foldl' (Map.unionWith sequenceConstraints) (unEvidence . singletonEvidence $ knownConstraints def def)
    . map unEvidence

sequenceConstraints :: Constraints -> Constraints -> Constraints
sequenceConstraints a b =
  Constraints
    { isStatic = isStatic a && isStatic b
    , isPure = isPure a && isPure b
    , knownValue = knownValue b
    , knownOutput = gappend <$> knownOutput a <*> knownOutput b
    }

inferTemplateConstraints :: Template a -> Infer Evidence
inferTemplateConstraints tpl = pure noEvidence

parseAndReportStmt :: String -> IO ()
parseAndReportStmt src = do
  let Template { templateBody = stmt } =
        either (error . formatParserError (Just src)) id
          . runIdentity
          . parseGinger (const . return $ Nothing) Nothing
          $ src
  let evidence = runInfer (inferStmt stmt) noEvidence
  mapM_ putStrLn . ppEvidence' $ fst evidence
  putStrLn "-----"
  mapM_ putStrLn . ppEvidence' $ snd evidence
