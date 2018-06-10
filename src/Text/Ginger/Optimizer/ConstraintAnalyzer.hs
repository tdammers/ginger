{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | A static analyzer for Ginger AST that infers the relative const-ness and
-- purity of Ginger constructs. That is, given constraint evidence for the
-- surrounding context, it can infer constraint evidence for a Ginger
-- expression, statement, or template.
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

-- ** Scope References

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

-- ** Constraints

-- | Knowledge that we have about the constraints on an individual item in a
-- scope.
--
-- Note that for the 'knownOutput' and 'knownValue' fields, 'Nothing' means
-- that we do not know what they evaluate to cq. render; so it most definitely
-- does not mean that they do not render any output, or evaluate to null. When
-- we actually do know that, the values will be 'Just def'.
data Constraints =
  Constraints
    { isStatic :: Bool -- ^ Value can be determined at compile time
    , isPure :: Bool -- ^ This is a pure function
    , knownValue :: Maybe (GVal Identity) -- ^ The static value, if known
    , knownOutput :: Maybe (GVal Identity) -- ^ Output, if known
    }
    deriving (Show)

-- | We know nothing about this item.
unconstrained :: Constraints
unconstrained =
  Constraints False False Nothing Nothing

-- | We know that we can determine the value of this item statically, but we
-- don't know what that value is yet.
staticConstraints :: Constraints
staticConstraints =
  unconstrained { isStatic = True }

-- | We know the value of this item, and also that is outputs nothing. Implies
-- 'isStatic'.
knownValueConstraints :: GVal Identity -> Constraints
knownValueConstraints val =
  knownConstraints val def

-- | We know the value of this item, and what it outputs. Implies 'isStatic'.
knownConstraints :: GVal Identity -> GVal Identity -> Constraints
knownConstraints val output =
  unconstrained
    { isStatic = True
    , knownValue = Just val
    , knownOutput = Just output
    }

-- | Additively combine constraints. The intuition is that
-- @appendConstraints a b@ means \"We already knew @a@, and now we have also
-- learned @b@.\"
appendConstraints :: Constraints -> Constraints -> Constraints
appendConstraints a b =
  Constraints
    { isStatic = isStatic a || isStatic b
    , isPure = isPure a || isPure b
    , knownValue = knownValue b <|> knownValue a
    , knownOutput = knownOutput b <|> knownOutput a
    }

-- ** Evidence

-- | The entire knowledge we have about a scope. Essentially, a lookup table
-- of 'ScopeRef's onto 'Constraints'.
newtype Evidence = Evidence { unEvidence :: Map ScopeRef Constraints }
  deriving (Show)

-- | The default 'Semigroup' on 'Evidence' is accumulating knowledge.
instance Semigroup Evidence where
  (<>) = appendEvidence

-- | The default 'Monoid' on 'Evidence' is accumulating knowledge, with \"we
-- don't know anything\" as the empty value.
instance Monoid Evidence where
  mempty = noEvidence
  mappend = (<>)

-- * Pretty-Printing

-- | Format a 'Constraints' record.
ppConstraints :: Constraints -> String
ppConstraints c =
  intercalate ", " $ catMaybes
    [ if isStatic c then Just "static" else Nothing
    , if isPure c then Just "pure" else Nothing
    , ("outputs: " ++) . show . htmlSource . asHtml <$> knownOutput c
    , ("value: " ++) . show . asText <$> knownValue c
    ]

-- | Format a 'ScopeRef'. The current scope is indicated as @.@, top-level
-- identifiers are printed as barewords, everything else is formatted as
-- square-bracket indexing.
ppRef :: ScopeRef -> String
ppRef CurrentItem = "."
ppRef (NamedItem n CurrentItem) = Text.unpack n
ppRef (NamedItem n parent) = ppRef parent ++ "[" ++ show n ++ "]"
ppRef (NumberedItem i CurrentItem) = "[" ++ show i ++ "]"
ppRef (NumberedItem i parent) = ppRef parent ++ "[" ++ show i ++ "]"

-- | Format an 'Evidence'; every scope item is returned as a separate list
-- element.
ppEvidence' :: Evidence -> [String]
ppEvidence' ev =
  [ ppRef ref ++ " is " ++ ppConstraints constraints
  | (ref, constraints) <- Map.toAscList (unEvidence ev)
  ]

-- | Format an 'Evidence'; every scope item is printed on its own line.
ppEvidence :: Evidence -> String
ppEvidence = unlines . ppEvidence'

-- * Manipulating Evidence

-- | Additively combine 'Evidence's.
appendEvidence :: Evidence -> Evidence -> Evidence
appendEvidence (Evidence a) (Evidence b) =
  Evidence $ Map.unionWith appendConstraints a b

-- | Alter the 'Constraints' at a given scope reference.
alterConstraints :: (Constraints -> Constraints) -> ScopeRef -> Evidence -> Evidence
alterConstraints f ref =
  Evidence
  . Map.alter
      (Just . f . fromMaybe unconstrained)
      ref
  . unEvidence

-- | Get the 'Constraints' at a given 'ScopeRef' in an 'Evidence'.
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

-- | Run an 'Infer' action over an initial 'Evidence'.
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

-- TODO:
-- CallE a (Expression a) [(Maybe Text, (Expression a))] -- ^ foo(bar=baz, quux)

-- TODO:
-- LambdaE a [Text] (Expression a) -- ^ (foo, bar) -> expr

inferExpr (TernaryE _ condE yesE noE) = do
  condEvidence <- inferExpr condE
  case knownValue (getConstraints CurrentItem condEvidence) of
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

-- | Infer the constraints on child expressions.
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
  gval <- knownValue constraints
  fmap (numberedItem . round) (asNumber gval)
    <|> Just (namedItem (asText gval))

-- | Infer the constraints on key/value pairs of child expressions.
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

-- | Infer constraints for a statement.
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

-- TODO:
-- IndentS a (Expression a) (Statement a) -- ^ Establish an indented context around the wrapped statement

inferStmt (LiteralS _ val) =
  -- Literals are always known
  pure . singletonEvidence $ knownConstraints def (toGVal val)

inferStmt (InterpolationS _ expr) = do
  exprEvidence <- inferExpr expr
  case knownValue (getConstraints CurrentItem exprEvidence) of
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
  case knownValue (getConstraints CurrentItem condEvidence) of
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

-- TODO:
-- SwitchS a (Expression a) [((Expression a), (Statement a))] (Statement a) -- ^ {% switch expression %}{% case expression %}statement{% endcase %}...{% default %}statement{% enddefault %}{% endswitch %}
-- ForS a (Maybe VarName) VarName (Expression a) (Statement a) -- ^ {% for index, varname in expression %}statement{% endfor %}

inferStmt (SetVarS _ name expr) = do
  exprEvidence <- inferExpr expr
  importEvidence (namedItem name) exprEvidence
  pure . singletonEvidence $ knownValueConstraints def

-- TODO:
-- DefMacroS a VarName (Macro a) -- ^ {% macro varname %}statements{% endmacro %}
-- BlockRefS a VarName
-- PreprocessedIncludeS a (Template a) -- ^ {% include "template" %}

inferStmt (NullS _) =
  pure . singletonEvidence $ knownValueConstraints def
-- TryCatchS a (Statement a) [CatchBlock a] (Statement a) -- ^ Try / catch / finally
inferStmt stmt = pure noEvidence

-- | Fold a list of 'Evidence's following sequential execution semantics. See
-- 'sequenceConstraints' for an explanation.
sequentialEvidence :: [Evidence] -> Evidence
sequentialEvidence =
  Evidence
    . foldl' (Map.unionWith sequenceConstraints) (unEvidence . singletonEvidence $ knownConstraints def def)
    . map unEvidence

-- | Combine two 'Evidence's following sequential execution semantics. This
-- means that staticnessand purity are combined subtractively (if both items
-- meet the constraint, then we retain it, otherwise we drop it), the known
-- value will be the value of the second argument, if any (because in
-- sequential
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
