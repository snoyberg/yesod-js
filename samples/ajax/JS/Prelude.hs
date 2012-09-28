{-# LANGUAGE OverloadedStrings #-}
module JS.Prelude where

import JS.Types
import JS.Render ()
import Data.Text (Text)
import Yesod.Core (lift, getUrlRenderParams, Route)

-- | Call the given function on a single argument
appFunc1 :: JSVarName -> JSExp a -> JSExp b
appFunc1 name arg = JSVarE name `JSAppE` Arg arg

alert :: JSExp Text -> JSExp ()
alert = appFunc1 "alert"

stringify :: JSExp a -> JSExp Text
stringify = appFunc1 "JSON.stringify"

data AjaxSettings input output = AjaxSettings

ajaxSettings :: JSExp input
             -> (JSExp output -> JSBody ())
             -> (Route master, [(Text, Text)])
             -> JS master (JSExp (AjaxSettings input output))
ajaxSettings input success url = do
    render <- lift $ lift getUrlRenderParams
    return $ JSObject
        [ JSPair "contentType" "application/json"
        , JSPair "data" $ stringify input
        , JSPair "success" $ JSLambda $ success . unArg
        , JSPair "error" $ JSLambda $ \() -> JSStmtExp $ alert "Some error occurred"
        , JSPair "type" "POST"
        , JSPair "url" $ JSText $ uncurry render url
        ]

ajax :: JSExp (AjaxSettings input output) -> JSExp ()
ajax = appFunc1 "$.ajax"

data JQuery = JQuery

jquery :: JSExp Text -> JSExp JQuery
jquery t = JSVarE "$" `JSAppE` Arg t

click :: JSExp JQuery -> JSBody () -> JSExp ()
click jq action = JSProperty jq "click" `JSAppE` Arg (JSLambda (\() -> action))

getValue :: JSExp JQuery -> JSExp Text
getValue jq = JSProperty jq "val" `JSAppE` ()

intToText :: JSExp Int -> JSExp Text
intToText i = JSProperty i "toString" `JSAppE` ()

onLoad :: JSBody () -> JSExp ()
onLoad f = "$" `appFunc1` JSLambda (\() -> f)
