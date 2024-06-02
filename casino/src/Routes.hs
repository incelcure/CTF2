module Routes where

import Foundation
import Yesod.Core

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Minimal Multifile"
  [whamlet|
        <p> hey
    |]
