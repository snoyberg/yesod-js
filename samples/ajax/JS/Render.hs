{-# LANGUAGE TypeFamilies, FlexibleInstances, GADTs, OverloadedStrings #-}
module JS.Render where

import JS.Types
import Data.Monoid ((<>), mconcat)
import Control.Monad.Trans.RWS (get, put)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, fromText, fromLazyText)
import Data.List (intersperse)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Aeson (encode)
import Yesod.Core
import Control.Monad.Trans.RWS (runRWST)
import Text.Julius (Javascript (Javascript))

runJS :: JS master (JSExp a) -> GWidget master master ()
runJS x = do
    render <- lift getUrlRenderParams
    (body, _, _) <- runRWST (x >>= showJSExp) render 1
    toWidget $ const $ Javascript body

getVarName :: JS master Text
getVarName = do
    i <- get
    put $ i + 1
    return $ "var" <> fromString (show i)

instance Args () where
    type ArgsType () = ()
    showArgs () = return []
    showLambda f = do
        f' <- showJSBody $ f ()
        return $ "function(){" <> f' <> "}"
instance Args (Arg a) where
    type ArgsType (Arg a) = a
    showArgs (Arg a) = sequence [showJSExp a]
    showLambda f = do
        varName <- getVarName
        f' <- showJSBody $ f $ Arg $ JSVarE $ JSVarName varName
        return $ "function(" <> fromText varName <> "){" <> f' <> "}"
instance Args (JSExp a, JSExp b) where
    type ArgsType (JSExp a, JSExp b) = (a, b)
    showArgs (a, b) = sequence [showJSExp a, showJSExp b]
    showLambda f = do
        var1 <- getVarName
        var2 <- getVarName
        f' <- showJSBody $ f (JSVarE $ JSVarName var1, JSVarE $ JSVarName var2)
        return $ "function(" <> fromText var1
                             <> "," <> fromText var2
                             <> "){" <> f' <> "}"

showJSBody :: JSBody a -> JS master Builder
showJSBody (JSStmtNoBind x y) = do
    x' <- showJSExp x
    y' <- showJSBody y
    return $ x' <> ";" <> y'
showJSBody (JSStmtBind x y) = do
    var <- getVarName
    x' <- showJSExp x
    y' <- showJSBody $ y $ JSVarE $ JSVarName var
    return $ "var " <> fromText var <> "=" <> x' <> ";" <> y'
showJSBody (JSStmtExp x) = do
    x' <- showJSExp x
    return $ "return " <> x' <> ";"

showJSExp :: JSExp a -> JS master Builder
showJSExp (JSText t) = return $ fromString $ show t
showJSExp (JSVarE (JSVarName t)) = return $ fromText t
showJSExp (JSAppE f x) = do
    f' <- showJSExp f
    x' <- showArgs x
    return $ f' <> "(" <> mconcat (intersperse "," x') <> ")"
showJSExp (JSValue value) = return $ fromLazyText $ decodeUtf8 $ encode value
showJSExp (JSLambda f) = showLambda f
showJSExp (JSObject pairs) = do
    pairs' <- mapM goPair pairs
    return $ "{" <> mconcat (intersperse "," pairs') <> "}"
  where
    goPair (JSPair key value) = do
        key' <- showJSExp key
        value' <- showJSExp value
        return $ key' <> ":" <> value'
showJSExp (JSProperty x p) = do
    x' <- showJSExp x
    return $ x' <> "." <> fromText p

instance IsString JSVarName where
    fromString = JSVarName . fromString
instance a ~ Text => IsString (JSExp a) where
    fromString = JSText . fromString
