{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
module Yesod.Javascript
    ( JS
    , JSValue
    , JSBody
    , JSFunc
    , textInput
    , textOutput
    , htmlOutput
    , jsPlus
    , runJS
    , ajaxJson

    , wrapTag
    , wrapTagClass
    , jsfor
    , jsjoin

    , jsToHtml

    , jsGetter

    , jsif

    , button
    , alert

    , jsShowInt
    , jslength

    , putJson
    , jsonObject
    , jsCast
    , jsTrue
    , jsFalse
    ) where

import Yesod.Core
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Control.Applicative ((<$>))
import Data.Monoid
import Control.Monad.Trans.Writer
import Text.Julius (Javascript (Javascript))
import Yesod.Form.Jquery (YesodJquery, urlJqueryJs)
import Data.Maybe (fromMaybe)
import Text.Blaze (Html)
import Data.List (intersperse)

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data JSValue jstype = JSValue
    { jsvExpr :: Builder
    , jsvDeps :: Set.Set JSVar
    }

newtype JSBody = JSBody Builder

newtype JSVar = JSVar Text
    deriving (Ord, Eq, Show)

newtype JSFunc = JSFunc Text
    deriving (Ord, Eq, Show)

instance (Text ~ jstype) => IsString (JSValue jstype) where
    fromString s = JSValue
        { jsvExpr = fromText $ T.pack $ show s -- FIXME JS-specific escaping via aeson
        , jsvDeps = Set.empty
        }

class JSPlus a
instance JSPlus Text

jsPlus :: JSPlus jstype => JSValue jstype -> JSValue jstype -> JSValue jstype
jsPlus (JSValue a x) (JSValue b y) = JSValue (a <> "+" <> b) (x <> y)

type JS sub master = WriterT JSData (GWidget sub master)

runJS :: YesodJquery master => JS sub master a -> GWidget sub master a
runJS js = do
    y <- lift getYesod
    addScriptEither $ urlJqueryJs y
    addScriptRemote "http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.3.1/underscore-min.js"

    (a, jsd) <- runWriterT js
    toWidget $ const $ Javascript $ go jsd
    return a
  where
    go :: JSData -> Builder
    go (JSData events vars funcs deps) =
        "$(function(){" <> varDecls <> funcDecls <> eventDecls <> "});"
      where
        varDecls = mconcat $ map varDecl $ Set.toList vars
        varDecl (JSVar t) = "var " <> fromText t <> ";"

        funcDecls = mconcat $ map funcDecl $ Map.toList funcs
        funcDecl (JSFunc name, body) =
            "var " <> fromText name <> "=function(){" <> body <> "};"

        eventDecls = mconcat $ map eventDecl $ Map.toList events
        eventDecl (var, mkBody) =
            mkBody funcs
          where
            funcs = fromMaybe Set.empty $ Map.lookup var deps

data JSData = JSData
    { jsdEvents :: Map.Map JSVar (Set.Set JSFunc -> Builder)
    , jsdVars :: Set.Set JSVar
    , jsdFuncs :: Map.Map JSFunc Builder
    , jsdDeps :: Map.Map JSVar (Set.Set JSFunc)
    }

instance Monoid JSData where
    mempty = JSData mempty mempty mempty mempty
    mappend (JSData a b c d) (JSData w x y z) = JSData
        (a <> w)
        (b <> x)
        (c <> y)
        (Map.unionWith mappend d z)

textInput :: JS sub master (GWidget sub master (), JSValue Text)
textInput = do
    varname <- lift $ lift newIdent
    id' <- lift $ lift newIdent
    let var = JSVar varname
        expr = "(" <> fromText varname <> "||''" <> ")"
        val = JSValue expr $ Set.singleton var
    let callFunc (JSFunc t) = ";" <> fromText t <> "()"
    tell $ mempty
        { jsdEvents = Map.singleton var $ \funcs ->
            "$(\"#" <> fromText id' <> "\").keyup(function(){" <>
            fromText varname <> "=$(this).val()" <> mconcat (map callFunc $ Set.toList funcs) <>
            "});"
        , jsdVars = Set.singleton var
        }
    let w = [whamlet|<input ##{id'} type=text>|]
    return (w, val)

textOutput :: JSValue Text -> JS sub master (GWidget sub master ())
textOutput (JSValue expr deps) = do
    funcname <- lift $ lift newIdent
    id' <- lift $ lift newIdent
    let func = JSFunc funcname
    tell $ mempty
        { jsdFuncs = Map.singleton func $
            "$(\"#" <> fromText id' <> "\").text(" <> expr <> ")"
        , jsdDeps = mconcat $ map (\var -> Map.singleton var $ Set.singleton func) $ Set.toList deps
        }
    return [whamlet|<span ##{id'}>|]

htmlOutput :: JSValue Html -> JS sub master (GWidget sub master ())
htmlOutput (JSValue expr deps) = do
    funcname <- lift $ lift newIdent
    id' <- lift $ lift newIdent
    let func = JSFunc funcname
    tell $ mempty
        { jsdFuncs = Map.singleton func $
            "$(\"#" <> fromText id' <> "\").html(" <> expr <> ")"
        , jsdDeps = mconcat $ map (\var -> Map.singleton var $ Set.singleton func) $ Set.toList deps
        }
    return [whamlet|<div ##{id'}>|]

wrapTag :: Text -> JSValue Html -> JSValue Html
wrapTag tag (JSValue expr deps) =
    JSValue expr' deps
  where
    expr' = "'<" <> fromText tag <> ">'+" <> expr <> "+'</" <> fromText tag <> ">'"

wrapTagClass :: Text -> JSValue Text -> JSValue Html -> JSValue Html
wrapTagClass tag (JSValue clazze clazzd) (JSValue expr deps) =
    JSValue expr' (clazzd <> deps)
  where
    expr' = "'<" <> fromText tag <> " class=\"'+" <> clazze <>
            "+'\">'+" <> expr <> "+'</" <> fromText tag <> ">'"

ajaxJson :: Route master -> JS sub master (JSValue jstype, JSFunc)
ajaxJson route = do
    render <- lift $ lift getUrlRender

    varname <- lift $ lift newIdent
    let var = JSVar varname

    loadFuncName <- lift $ lift newIdent
    let loadFunc = JSFunc loadFuncName

    tell $ mempty
        { jsdVars = Set.singleton var
        , jsdEvents = Map.singleton var $ \fs ->
            "var " <> fromText loadFuncName <> "=function(){$.getJSON('" <>
            fromText (render route) <> "', function(data){" <>
            fromText varname <> "=data;" <> callFuncs fs <> "})};" <>
            fromText loadFuncName <> "();"
        }

    return (JSValue
        { jsvExpr = fromText varname
        , jsvDeps = Set.singleton var
        }, loadFunc)
  where
    callFuncs = mconcat . map (\(JSFunc f) -> fromText f <> "();") . Set.toList

jsfor :: JSValue [a] -> (JSValue a -> JSValue b) -> JSValue [b]
jsfor (JSValue expr deps) f =
    JSValue expr' deps'
  where
    expr' = "_.map(" <> expr <> ", function(x){return " <> fexpr <> "})"
    deps' = deps <> fdeps
    JSValue fexpr fdeps = f a
    a = JSValue "x" mempty

jsjoin :: JSValue [Html] -> JSValue Html
jsjoin (JSValue expr deps) =
    JSValue expr' deps
  where
    expr' = expr <> ".join('')"

jsToHtml :: JSValue Text -> JSValue Html
jsToHtml (JSValue expr deps) =
    JSValue expr' deps
  where
    expr' = expr -- FIXME entity escaping

jsGetter :: Text -> JSValue a -> JSValue b
jsGetter name (JSValue expr deps) =
    JSValue expr' deps
  where
    expr' = expr <> "." <> fromText name

jsif :: JSValue Bool -> JSValue a -> JSValue a -> JSValue a
jsif (JSValue cond d1) (JSValue t d2) (JSValue f d3) =
    JSValue expr (d1 <> d2 <> d3)
  where
    expr = "(" <> cond <> "?" <> t <> ":" <> f <> ")"

button :: GWidget sub master () -> JSBody -> JS sub master (GWidget sub master ())
button inside (JSBody body) = do
    id' <- lift $ lift newIdent
    let var = JSVar id' -- just a hack
    tell mempty
        { jsdEvents = Map.singleton var $ \_ ->
            "$('#" <> fromText id' <> "').click(function(){" <> body <> "return false});"
        }
    return [whamlet|<button id=#{id'}>^{inside}|]

alert :: JSValue Text -> JSBody
alert (JSValue expr _) = JSBody $ "alert(" <> expr <> ");"

jslength :: JSValue [a] -> JSValue Int
jslength (JSValue expr deps) = JSValue (expr <> ".length") deps

jsShowInt :: JSValue Int -> JSValue Text
jsShowInt (JSValue expr deps) = JSValue (expr <> ".toString()") deps

jsCast :: JSValue a -> JSValue b
jsCast (JSValue e d) = (JSValue e d)

jsonObject :: [(Text, JSValue a)] -> JSValue b
jsonObject pairs =
    JSValue expr deps
  where
    deps = mconcat $ map (jsvDeps . snd) pairs
    exprs = map (\(key, JSValue e _) -> fromText (T.pack $ show key) <> ":" <> e) pairs
    expr = "{" <> mconcat (intersperse "," exprs) <> "}"

jsTrue :: JSValue Bool
jsTrue = JSValue "true" mempty

jsFalse :: JSValue Bool
jsFalse = JSValue "false" mempty

putJson :: Route master -> JSValue a -> JSFunc -> JS sub master JSBody
putJson url (JSValue expr _) (JSFunc func) = do
    render <- lift $ lift getUrlRender
    return $ JSBody $
        "$.ajax({type:'PUT',url:'" <> fromText (render url) <> "',data:JSON.stringify(" <>
        expr <> "),processData:false,success:" <> fromText func <> "});"
