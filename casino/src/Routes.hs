module Routes where

import Data.Casino.User
import Database.Persist
import Foundation
import Yesod.Auth
import Yesod.Core

onclick :: JavascriptUrl (Route App)
onclick =
  [julius|
    fetch('@{SpinR}', {
      method: 'POST'
    }).then(r => r.text())
      .then(d => ^{getSpins})
      .catch(error => console.error('Error:', error))
    |]

getSpins :: JavascriptUrl (Route App)
getSpins =
  [julius|
    fetch('@{SpinsR}', {
      method: 'GET'
    }).then(r => r.json())
      .then(d => document.getElementById('spins').innerText =
        `you have ${d.count} spins left`
      )
      .catch(error => console.error('Error:', error))
    |]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Casino"
  _ <- maybe (permissionDenied "") (return . entityVal) =<< maybeAuth
  r <- getUrlRenderParams
  [whamlet|
      <p #spins>
      <button onclick="#{renderJavascriptUrl r onclick}">
        Spin!
      <p>
        <a href=@{AuthR LogoutR}>Logout
    |]
  toWidget getSpins
