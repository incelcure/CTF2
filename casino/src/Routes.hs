module Routes where

import Database.Persist
import Foundation
import Prometheus
import Yesod.Auth
import Yesod.Core

getMetricsR :: Handler Text
getMetricsR = decodeUtf8 <$> liftIO exportMetricsAsText

onclick :: JavascriptUrl (Route App)
onclick =
  [julius|
    fetch('@{SpinR}', {
      method: 'POST'
    }).then(r => r.json())
      .then(d => {
        let li = document.createElement('li')
        if (d === null)
          li.innerHTML = `You got nothing`
        else
          li.innerHTML = `You got ${d.rewardType}: <span style="color: ${d.rewardValue};">${d.rewardValue}</spin>`
        document.getElementById('rewards').prepend(li)
        ^{getSpins}
      })
      .catch(error => console.error('Error:', error))
    |]

getSpins :: JavascriptUrl (Route App)
getSpins =
  [julius|
    fetch('@{SpinsR}', {
      method: 'GET'
    }).then(r => r.json())
      .then(d => document.getElementById('spins').innerText =
        `You have ${d.count} spins left`
      )
      .catch(error => console.error('Error:', error))
    |]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  _ <- maybe (permissionDenied "") (return . entityVal) =<< maybeAuth
  setTitle "Casino"
  r <- getUrlRenderParams
  [whamlet|
      <p #spins>
      <button onclick="#{renderJavascriptUrl r onclick}">
        Spin!
      <ul #rewards>
      <p>
        <a href=@{AuthR LogoutR}>Logout
    |]
  toWidget getSpins
  addStylesheet $ StaticR styles_css
