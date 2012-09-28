{-# LANGUAGE GADTs, TypeFamilies #-}
module JS.Types where

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder, fromText)
import Yesod.Core (GWidget, Route)
import Data.Text (Text)
import Control.Monad.Trans.RWS (RWST)
import Data.Aeson (ToJSON)

-- Javascript representation, tagged via phantom types.

newtype JSVarName = JSVarName Text

type RenderUrl url = url -> [(Text, Text)] -> Text
type JS master = RWST (RenderUrl (Route master)) () Int (GWidget master master)

data JSBody result where
    JSStmtNoBind :: JSExp a -> JSBody result -> JSBody result
    JSStmtBind :: JSExp a -> (JSExp a -> JSBody result) -> JSBody result
    JSStmtExp :: JSExp a -> JSBody a

data JSExp a where
    JSLambda :: Args a => (a -> JSBody b) -> JSExp (ArgsType a -> b)
    JSText :: Text -> JSExp Text
    JSValue :: ToJSON value => value -> JSExp value
    JSVarE :: JSVarName -> JSExp a
    JSAppE :: Args a => JSExp (ArgsType a -> b) -> a -> JSExp b
    JSObject :: [JSPair] -> JSExp a
    JSProperty :: JSExp a -> Text -> JSExp b

class Args a where
    type ArgsType a
    showArgs :: a -> JS master [Builder]
    showLambda :: (a -> JSBody b) -> JS master Builder
data Arg a = Arg { unArg :: JSExp a }


data JSPair where
    JSPair :: JSExp Text -> JSExp a -> JSPair
