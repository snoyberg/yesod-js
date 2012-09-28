{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

import Yesod
import Command
import JS.Prelude
import JS.Types
import JS.Render
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode)
import Yesod.Form.Jquery
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.String
import Control.Monad.Trans.RWS
import Data.Text.Lazy.Builder (Builder, fromText, fromLazyText, toLazyText)
import Data.List (intersperse)
import Data.Monoid ((<>))

data Ajax = Ajax

mkYesod "Ajax" [parseRoutes|
/ HomeR GET
/command CommandR POST
|]

instance Yesod Ajax
instance YesodJquery Ajax
instance RunsCommand Ajax where
    commandRoute = CommandR

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Ajax Demo"
    y <- lift getYesod
    addScriptEither $ urlJqueryJs y
    runJS js
    [whamlet|
Name:
<input type=text id=name placeholder=name>
<button .btn id=hello>Hello
<button .btn id=count>Count
|]
  where
    js = do
        hello <- runCommandClient
            (helloInputJS $ getValue $ jquery "#name")
            (JSStmtExp . alert . hoContentJS)
            Hello
        count <- runCommandClient
            (countInputJS $ getValue $ jquery "#name")
            (\co -> do
                JSStmtNoBind (alert "Going to show you the length")
                $ JSStmtExp $ alert $ intToText $ coLengthJS co
            )
            Count
        let hello' = jquery "#hello" `click` JSStmtExp hello
        let count' = jquery "#count" `click` JSStmtExp count
        return $ onLoad $ JSStmtNoBind hello' $ JSStmtExp count'

postCommandR :: Handler RepJson
postCommandR = runCommandServer CommandServer
    { serveHello = \(HelloInput name) -> return $ HelloOutput $ "Hello, " <> name <> "!"
    , serveCount = \(CountInput word) -> return $ CountOutput $ T.length word
    }

main :: IO ()
main = warpDebug 3000 Ajax
