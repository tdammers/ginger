{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}
module Text.Ginger.Compile.JS
where

import Data.HashMap.Strict as HashMap
import Text.Ginger.AST
import Text.Ginger.Html
import Control.Monad (forM_)
import Data.Text.IO as Text
import Data.Text as Text
import Data.Text (Text)
import Data.FileEmbed
import Debug.Trace (traceM)

class OutputStream m where
    out :: Text -> m ()

instance OutputStream IO where
    out = Text.putStr

jsPrelude :: Text
jsPrelude = $(embedStringFile "src/Text/Ginger/Compile/JS/prelude.js")

compileTemplate :: (Monad m, OutputStream m) => Template -> m ()
compileTemplate tpl = do
    out "module.exports = function(write, encode, context) {\n"
    out jsPrelude
    out "var context = $prelude.mergeObjects($prelude.defaultContext, context)\n"
    out "var blocks = {};\n"
    compileTemplateBlocks tpl
    traceM . show $ templateBody tpl
    compileTemplateBody tpl
    out "}\n"

compileTemplateBlocks :: (Monad m, OutputStream m) => Template -> m ()
compileTemplateBlocks tpl = do
    maybe (pure ()) compileTemplateBlocks $
        templateParent tpl
    forM_ (HashMap.toList $ templateBlocks tpl) $ \(name, block) -> do
        out "blocks."
        out name
        out " = function() {"
        compileStatement (blockBody block)
        out "}\n"

compileTemplateBody :: (Monad m, OutputStream m) => Template -> m ()
compileTemplateBody tpl =
    case templateParent tpl of
        Nothing -> compileStatement (templateBody tpl)
        Just p -> compileTemplateBody tpl

compileStatement :: (Monad m, OutputStream m) => Statement -> m ()
compileStatement NullS =
    out "''\n"
compileStatement (MultiS ss) =
    forM_ ss compileStatement
compileStatement (InterpolationS e) = do
    out "write(encode("
    compileExpression e
    out "))\n"
compileStatement (LiteralS h) = do
    out "write("
    outShow $ htmlSource h
    out ")\n"
compileStatement (SetVarS n e) = do
    out "context["
    outShow n
    out "] = "
    compileExpression e
    out "\n"
compileStatement (IfS cond yes no) = do
    out "if ("
    compileExpression cond
    out ") {\n"
    compileStatement yes
    out "}\n"
    out "else {\n"
    compileStatement no
    out "}\n"
compileStatement (ForS keyNameMay varName iteree body) = do
    out ";(function (parentContext) {\n"
    out "var context = $prelude.mergeObjects(parentContext, {})\n"
    out "$prelude.iterate("
    compileExpression iteree
    out ", function($iterkey, $iterval) {\n"
    out "context["
    outShow varName
    out "] = $iterval\n"
    case keyNameMay of
        Nothing -> pure ()
        Just keyName -> do
            out "context["
            outShow keyName
            out "] = $iterkey\n"
    compileStatement body
    out "})\n"
    out "})(context)\n"
compileStatement s = do
    out "/* UNSUPPORTED: "
    outShow s
    out " */'';\n"

outShow :: Show a
        => Monad m
        => OutputStream m
        => a
        -> m ()
outShow = out . Text.pack . show

compileExpression :: (Monad m, OutputStream m) => Expression -> m ()
compileExpression (StringLiteralE str) = do
    outShow str
compileExpression (NumberLiteralE n) = do
    outShow n
compileExpression (BoolLiteralE True) =
    out "true"
compileExpression (BoolLiteralE False) =
    out "false"
compileExpression NullLiteralE =
    out "null"
compileExpression (MemberLookupE c k) = do
    out "$prelude.lookup("
    compileExpression c
    out ", "
    compileExpression k
    out ")"
compileExpression (VarE varName) = do
    out "$prelude.lookup(context, "
    outShow varName
    out ")"
compileExpression (ListE items) = do
    out "["
    case items of
        [] -> pure ()
        (x:xs) -> do
            compileExpression x
            forM_ xs $ \x -> do
                out ", "
                compileExpression x
    out "]"
compileExpression (ObjectE keyvals) = do
    out "(function() {\n"
    out "var $result = {}\n"
    forM_ keyvals $ \(key, val) -> do
        out "$result["
        compileExpression key
        out "] = "
        compileExpression val
        out "\n"
    out "return $result\n"
    out "})()"
compileExpression (TernaryE c y n) = do
    out "("
    compileExpression c
    out " ? "
    compileExpression y
    out " : "
    compileExpression n
    out ")"
compileExpression (CallE f args) = do
    compileExpression f
    out "("
    let positionalArgs = [ a | (Nothing, a) <- args ]
        namedArgs = [ (name, a) | (Just name, a) <- args ]
    case positionalArgs of
        [] -> pure ()
        (x:xs) -> do
            compileExpression x
            forM_ xs $ \x -> do
                out ", "
                compileExpression x
    out ")"
compileExpression (LambdaE argNames body) = do
    out "(function () {"
    out "(function (parentContext) {\n"
    out "var context = $prelude.mergeObjects(parentContext, {})\n"
    out "var args = [].slice.call(arguments)\n"
    out "for (var i = 0; i < argNames.length; ++i) {\n"
    out "context[argNames[i]] = arguments[i] || null\n"
    out "}\n"
    compileExpression body
    out "})(context)\n"
    out "})\n"


compileExpression e = do
    out "null /* UNSUPPORTED: "
    outShow e
    out " */"
