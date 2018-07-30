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

-- | @makeRefRelativeTo candidate parent@ attempts to remove the @parent@ ref
-- from the @candidate@, if @candidate@ is indeed inside the @parent@ scope.
makeRefRelativeTo :: ScopeRef -- ^ candidate
                  -> ScopeRef -- ^ parent
                  -> Maybe ScopeRef -- ^ 'Just' the relative part, or 'Nothing'
                                    -- if @candidate@ is not in @parent@
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

-- | We know that it's a pure function, which also implies staticness.
pureConstraints :: Constraints
pureConstraints =
  staticConstraints { isPure = True }

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
    , knownValue = knownValue b ||? knownValue a
    , knownOutput = knownOutput b ||? knownOutput a
    }

-- | Intersection of constraints.
-- @intersectConstraints a b@ gives the facts that hold under both sets of
-- constraints.
intersectConstraints :: Constraints -> Constraints -> Constraints
intersectConstraints a b =
  Constraints
    { isStatic = isStatic a &&isStatic b
    , isPure = isPure a && isPure b
    , knownValue = knownValue b &&? knownValue a
    , knownOutput = knownOutput b &&? knownOutput a
    }

-- | Union of 'Maybe's
(||?) :: Maybe a -> Maybe a -> Maybe a
Nothing ||? b = b
a ||? _ = a

-- | Intersection of 'Maybe's
(&&?) :: Maybe a -> Maybe a -> Maybe a
Nothing &&? b = Nothing
a &&? Nothing = Nothing
a &&? b = a

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
ppConstraints Constraints { isStatic = False, isPure = False } =
  "unconstrained"
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

-- | Merge 'Evidence's such that constraints on identifiers found in both
-- sets of evidence are combined by intersection. This represents a situation
-- where constraints from two lines of analysis must both hold. For any given
-- key in either evidence map, we have to intersect the constraints with those
-- found for the same key in the other map (because *both* sets of constraints
-- must hold), but if the key doesn't exist in the other map, then we know that
-- that line of analysis didn't encounter the key at all, and thus the
-- constraints we have will all hold (iow., the code analysed in the other line
-- of analysis doesn't touch the key at all, so it doesn't influence the
-- constraints).
intersectUnionEvidence :: Evidence -> Evidence -> Evidence
intersectUnionEvidence (Evidence a) (Evidence b) =
  Evidence $ Map.unionWith intersectConstraints a b

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
        singletonEvidence
        (Map.foldl' appendConstraints (knownConstraints def def) .
          unEvidence $
          childEvidence)
        { knownValue =
            toGVal <$>
              (mapM knownValue . Map.elems . unEvidence $ childEvidence)
        }
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

inferExpr (CallE _ funcE argsE) = do
  funcConstraints <- getConstraints CurrentItem <$> inferExpr funcE
  case funcConstraints of
    -- Constraints { isStatic = True, isPure = True, knownValue = Just func } -> do
      -- We know the actual function, and it's pure. This is very good.
    Constraints { isStatic = True, isPure = True } -> do
      -- We know it's a pure function, and while we don't know which exact one,
      -- we still know that iff all the arguments are static, then so is the
      -- return value.
      constraints <- forM argsE $ \(_, argE) -> do
        getConstraints CurrentItem <$> inferExpr argE
      pure . singletonEvidence $ unconstrained { isStatic = all isStatic constraints }
    _ ->
      -- Not a pure function: all bets are off
      pure noEvidence


inferExpr (LambdaE _ argNames body) = do
  applyEvidence <- withLocalState $ do
    -- We'll infer the body in a hypothetical context where all the
    -- arguments are pure and static. If the function is pure, then
    -- the result of this will indicate staticness.
    forM argNames $ \argName ->
      importEvidence
        (namedItem argName)
        (singletonEvidence pureConstraints)
    inferExpr body
  let applyConstraints = getConstraints CurrentItem applyEvidence
  if isStatic applyConstraints then
    pure . singletonEvidence $ pureConstraints
  else
    pure . singletonEvidence $ unconstrained

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

-- | @withLocalState action@ runs @action@ against a copy of the current state,
-- leaving the actual state untouched. Any changes to the state made from
-- within @action@ are discarded on the outside.
withLocalState :: State s a -> State s a
withLocalState a = do
  s <- get
  a <* put s

namedRefList :: [(Text, a)] -> [(ScopeRef, a)]
namedRefList = map (\(k, v) -> (namedItem k, v))

numberedRefList :: [a] -> [(ScopeRef, a)]
numberedRefList = map (\(k, v) -> (numberedItem k, v)) . zip [0..]

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

inferStmt (ScopedS _ body) =
  -- For a scoped statement, we simply infer the body in a local state.
  withLocalState $ inferStmt body

inferStmt (IndentS _ expr body) = do
  exprEvidence <- inferExpr expr
  bodyEvidence <- inferStmt body
  return $ intersectUnionEvidence exprEvidence bodyEvidence

inferStmt (LiteralS _ val) =
  -- Literals are always known
  pure . singletonEvidence $ knownConstraints def (toGVal val)

inferStmt (InterpolationS _ expr) = do
  -- An interpolation statement is static iff its expressions is
  -- static; it always returns null, and its output is the value
  -- of the expression.
  exprEvidence <- inferExpr expr
  let exprConstraints = getConstraints CurrentItem exprEvidence
  pure . singletonEvidence $
    unconstrained
      { isStatic = isStatic exprConstraints
      , knownOutput = knownValue exprConstraints
      , knownValue = Just def
      }

inferStmt (ExpressionS _ expr) = do
  -- An expression statement merely evaluates the expression for its
  -- side effects
  inferExpr expr
  pure . singletonEvidence $ knownConstraints def def

inferStmt (IfS _ condExpr yesStmt noStmt) = do
  condEvidence <- inferExpr condExpr
  case knownValue (getConstraints CurrentItem condEvidence) of
    Nothing -> do
      -- We have to assume that both the yes and the no branch may run, so
      -- the best we can do is report the evidence intersection of the
      -- expression and both branches.
      yesEv <- inferStmt yesStmt
      noEv <- inferStmt noStmt
      pure $ condEvidence `intersectUnionEvidence` yesEv `intersectUnionEvidence` noEv
    Just val ->
      case asBoolean val of
        True -> inferStmt yesStmt
        False -> inferStmt noStmt

-- TODO:
-- SwitchS a (Expression a) [((Expression a), (Statement a))] (Statement a) -- ^ {% switch expression %}{% case expression %}statement{% endcase %}...{% default %}statement{% enddefault %}{% endswitch %}

inferStmt (ForS _ varNameIndexMay varNameValue itereeE body) = do
  -- for loops work as follows.
  --
  -- First, we need to figure out the constraints on the iteree.
  itereeEvidence <- inferExpr itereeE
  let itereeConstraints = getConstraints CurrentItem itereeEvidence
      -- remember what output evaluating the iteree generates
      itereeOutput = knownOutput itereeConstraints
  case itereeConstraints of
    -- If the iteree
    -- is not known statically, then we can't really say anything about the loop
    -- at all.
    Constraints { isStatic = False } ->
      pure noEvidence

    -- If the iteree *is* fully known, then we can unroll the loop.
    Constraints { knownValue = Just value } -> do
      let itereeValues =
            fromMaybe [] $
            (namedRefList <$> asDictItems value) <|>
            (numberedRefList <$> asList value)
      fmap sequentialEvidence . forM itereeValues $ \(ref, val) ->
        withLocalState $ do
          importEvidence CurrentItem $
                Evidence . Map.fromList . catMaybes $
                  [ Just (namedItem varNameValue, staticConstraints)
                  , do
                      varNameIndex <- varNameIndexMay
                      Just (namedItem varNameIndex, staticConstraints)
                  ]
          portEvidence ref <$> inferStmt body

    -- If it's static, but we don't know the value, then we can only do a
    -- general constness analysis on the body, but we cannot unroll the loop
    -- yet. We still know that *if* the body is static in a context where the
    -- loop index and loop variable are static, then the whole loop is also
    -- static.
    _ -> withLocalState $ do
      -- The loop variables axiomatically only depend on the iteree, and
      -- since we already know that the iteree is static, we are allowed to
      -- fabricate evidence on the loop variable(s).
      importEvidence CurrentItem $
            Evidence . Map.fromList . catMaybes $
              [ Just (namedItem varNameValue, staticConstraints)
              , do
                  varNameIndex <- varNameIndexMay
                  Just (namedItem varNameIndex, staticConstraints)
              ]
      -- And now we run the inference on the loop body.
      inferStmt body

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

parseAndReportExpr :: String -> IO ()
parseAndReportExpr src = do
  let Template { templateBody = InterpolationS _ expr } =
        either (error . formatParserError (Just src)) id
          . runIdentity
          . parseGinger (const . return $ Nothing) Nothing
          $ "{{ " ++ src ++ " }}"
  let evidence = runInfer (inferExpr expr) noEvidence
  mapM_ putStrLn . ppEvidence' $ fst evidence

parseAndReportStmt :: String -> IO ()
parseAndReportStmt src = do
  let Template { templateBody = stmt } =
        either (error . formatParserError (Just src)) id
          . runIdentity
          . parseGinger (const . return $ Nothing) Nothing
          $ src
  let evidence = runInfer (inferStmt stmt) noEvidence
  mapM_ putStrLn . ppEvidence' $ fst evidence
