{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
             TypeFamilies, OverloadedStrings
  #-}
import Yesod.Core
import Yesod.Javascript
import Network.Wai.Handler.Warp (run)
import Yesod.Form.Jquery (YesodJquery)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App
instance YesodJquery App

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ runJS $ do

    (firstWidget, firstVal) <- textInput
    (lastWidget, lastVal) <- textInput

    let fullVal = firstVal `jsPlus` " " `jsPlus` lastVal
    fullWidget <- textOutput fullVal

    lift [whamlet|
<p>First name: ^{firstWidget}
<p>Last name: ^{lastWidget}
<p>Full name: ^{fullWidget}
|]

main :: IO ()
main = toWaiApp App >>= run 3000
