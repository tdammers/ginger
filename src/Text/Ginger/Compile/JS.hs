{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}
module Text.Ginger.Compile.JS
where

import qualified Data.HashMap.Strict as HashMap
import Text.Ginger.AST
import Text.Ginger.Html
import Control.Monad (forM_)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Data.Text (Text)
import Data.FileEmbed (embedStringFile)
import Debug.Trace (traceM)
import Text.Wryte (wryte, wryteLn, Wryte, indented, runWryte_, defWryteOptions)
import Data.String (fromString)

jsPrelude :: String
jsPrelude = $(embedStringFile "src/Text/Ginger/Compile/JS/prelude.js")

printCompiled :: Wryte Text () -> IO ()
printCompiled a = Text.putStr $ runWryte_ defWryteOptions a

compileTemplate :: Template -> Wryte Text ()
compileTemplate tpl = do
    wryteLn "module.exports = function(write, encode, context) {"
    indented $ do
        mapM_ (wryteLn . fromString) $ lines jsPrelude
        wryteLn "var context = $prelude.mergeObjects($prelude.defaultContext, context)"
        wryteLn "var blocks = {};"
        compileTemplateBlocks tpl
        -- traceM . show $ templateBody tpl
        compileTemplateBody tpl
    wryteLn "}"

compileTemplateBlocks :: Template -> Wryte Text ()
compileTemplateBlocks tpl = do
    maybe (pure ()) compileTemplateBlocks $
        templateParent tpl
    forM_ (HashMap.toList $ templateBlocks tpl) $ \(name, block) -> do
        wryte "blocks."
        wryte name
        wryte " = function() {"
        compileStatement (blockBody block)
        wryteLn "}"

compileTemplateBody :: Template -> Wryte Text ()
compileTemplateBody tpl =
    case templateParent tpl of
        Nothing -> compileStatement (templateBody tpl)
        Just p -> compileTemplateBody tpl

compileStatement :: Statement -> Wryte Text ()
compileStatement NullS =
    wryteLn "''"
compileStatement (MultiS ss) =
    forM_ ss compileStatement
compileStatement (InterpolationS e) = do
    wryte "write(encode("
    compileExpression e
    wryteLn "))"
compileStatement (LiteralS h) = do
    wryte "write("
    wryteShow $ htmlSource h
    wryteLn ")"
compileStatement (SetVarS n e) = do
    wryte "context["
    wryteShow n
    wryte "] = "
    compileExpression e
    wryteLn ""
compileStatement (IfS cond yes no) = do
    wryte "if ("
    compileExpression cond
    wryteLn ") {"
    compileStatement yes
    wryteLn "}"
    wryteLn "else {"
    compileStatement no
    wryteLn "}"
compileStatement (ForS keyNameMay varName iteree body) = do
    wryteLn ";(function (parentContext) {"
    wryteLn "var context = $prelude.mergeObjects(parentContext, {})"
    wryte "$prelude.iterate("
    compileExpression iteree
    wryteLn ", function($iterkey, $iterval) {"
    wryte "context["
    wryteShow varName
    wryteLn "] = $iterval"
    case keyNameMay of
        Nothing -> pure ()
        Just keyName -> do
            wryte "context["
            wryteShow keyName
            wryteLn "] = $iterkey"
    compileStatement body
    wryteLn "})"
    wryteLn "})(context)"
compileStatement s = do
    wryte "/* UNSUPPORTED: "
    wryteShow s
    wryteLn " */'';"

wryteShow :: Show a => a -> Wryte Text ()
wryteShow = wryte . Text.pack . show

compileExpression :: Expression -> Wryte Text ()
compileExpression (StringLiteralE str) = do
    wryteShow str
compileExpression (NumberLiteralE n) = do
    wryteShow n
compileExpression (BoolLiteralE True) =
    wryte "true"
compileExpression (BoolLiteralE False) =
    wryte "false"
compileExpression NullLiteralE =
    wryte "null"
compileExpression (MemberLookupE c k) = do
    wryte "$prelude.lookup("
    compileExpression c
    wryte ", "
    compileExpression k
    wryte ")"
compileExpression (VarE varName) = do
    wryte "$prelude.lookup(context, "
    wryteShow varName
    wryte ")"
compileExpression (ListE items) = do
    wryte "["
    case items of
        [] -> pure ()
        (x:xs) -> do
            compileExpression x
            forM_ xs $ \x -> do
                wryte ", "
                compileExpression x
    wryte "]"
compileExpression (ObjectE keyvals) = do
    wryteLn "(function() {"
    wryteLn "var $result = {}"
    forM_ keyvals $ \(key, val) -> do
        wryte "$result["
        compileExpression key
        wryte "] = "
        compileExpression val
        wryteLn ""
    wryteLn "return $result"
    wryte "})()"
compileExpression (TernaryE c y n) = do
    wryte "("
    compileExpression c
    wryte " ? "
    compileExpression y
    wryte " : "
    compileExpression n
    wryte ")"
compileExpression (CallE f args) = do
    wryte "$prelude.callFunction("
    compileExpression f
    wryte ","
    wryte "["
    let positionalArgs = [ a | (Nothing, a) <- args ]
        namedArgs = [ (name, a) | (Just name, a) <- args ]
    case positionalArgs of
        [] -> pure ()
        (x:xs) -> do
            compileExpression x
            forM_ xs $ \x -> do
                wryte ", "
                compileExpression x
    wryte "]"
    wryte ")"
compileExpression (LambdaE argNames body) = do
    wryte "(function () {"
    wryteLn "(function (parentContext) {"
    wryteLn "var context = $prelude.mergeObjects(parentContext, {})"
    wryteLn "var args = [].slice.call(arguments)"
    wryteLn "for (var i = 0; i < argNames.length; ++i) {"
    wryteLn "context[argNames[i]] = arguments[i] || null"
    wryteLn "}"
    compileExpression body
    wryteLn "})(context)"
    wryteLn "})"


compileExpression e = do
    wryte "null /* UNSUPPORTED: "
    wryteShow e
    wryte " */"
