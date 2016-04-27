module Handler.Device where

import Import

getDevicesR :: Handler Html
getDevicesR = do
  userIdStringMaybe <- lookupGetParam "user_id"
  case (readMay =<< userIdStringMaybe) of
    Just userId -> do
      user <- runDB $ get404 userId
      devices <- runDB $ selectList [ DeviceUserId ==. userId ] []
      defaultLayout $(widgetFile "device/index")
    Nothing -> notFound

getDeviceCreateR :: Handler Html
getDeviceCreateR = do
  userIdStringMaybe <- lookupGetParam "user_id"
  case (readMay =<< userIdStringMaybe) of
    Just userId -> do
      (formWidget, formEnctype) <- generateFormPost $ deviceForm (Just userId)
      defaultLayout $(widgetFile "device/create")
    Nothing -> notFound

postDeviceCreateR :: Handler Html
postDeviceCreateR = do
  ((result, formWidget), formEnctype) <- runFormPost $ deviceForm Nothing
  case result of
    FormSuccess device -> do
      _ <- runDB $ insert device
      redirect (DevicesR, [("user_id", pack . show . deviceUserId $ device)])
    _ -> defaultLayout $(widgetFile "device/create")

getDeviceDeleteR :: DeviceId -> Handler Html
getDeviceDeleteR deviceId = do
  device <- runDB $ get404 deviceId
  _ <- runDB $ delete deviceId
  redirect (DevicesR, [("user_id", pack . show . deviceUserId $ device)])

deviceForm :: Maybe UserId -> Html -> MForm Handler (FormResult Device, Widget)
deviceForm userIdMaybe = renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 3)) $ Device
  <$> areq hiddenField "" userIdMaybe
  <*> areq textField (bfs ("MACアドレス" :: Text)) Nothing
  <*  bootstrapSubmit (BootstrapSubmit ("登録" :: Text) "btn-primary" [("attribute-name","attribute-value")])
