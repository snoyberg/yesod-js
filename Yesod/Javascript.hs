{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
module Yesod.Javascript
    ( JSString
    , JS
    , textInput
    , textOutput
    , jsPlus
    , runJS
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

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data JSValue jstype = JSValue
    { jsvExpr :: Builder
    , jsvDeps :: Set.Set JSVar
    }

newtype JSVar = JSVar Text
    deriving (Ord, Eq, Show)

newtype JSFunc = JSFunc Text
    deriving (Ord, Eq, Show)

data JSTypeString

type JSString = JSValue JSTypeString

instance (JSTypeString ~ jstype) => IsString (JSValue jstype) where
    fromString s = JSValue
        { jsvExpr = fromText $ T.pack $ show s -- FIXME JS-specific escaping via aeson
        , jsvDeps = Set.empty
        }

class JSPlus a
instance JSPlus JSTypeString

jsPlus :: JSPlus jstype => JSValue jstype -> JSValue jstype -> JSValue jstype
jsPlus (JSValue a x) (JSValue b y) = JSValue (a <> "+" <> b) (x <> y)

type JS sub master = WriterT JSData (GWidget sub master)

runJS :: YesodJquery master => JS sub master a -> GWidget sub master a
runJS js = do
    y <- lift getYesod
    addScriptEither $ urlJqueryJs y

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

textInput :: JS sub master (GWidget sub master (), JSString)
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

textOutput :: JSString -> JS sub master (GWidget sub master ())
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
