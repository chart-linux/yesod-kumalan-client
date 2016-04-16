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
  users <- runDB $ selectList [] []
  (formWidget, formEnctype) <- generateFormPost $ deviceForm (readMay =<< userIdStringMaybe) users
  defaultLayout $(widgetFile "device/create")

postDeviceCreateR :: Handler Html
postDeviceCreateR = do
  users <- runDB $ selectList [] []
  ((result, formWidget), formEnctype) <- runFormPost $ deviceForm Nothing users
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

deviceForm :: Maybe UserId -> [Entity User] -> Html -> MForm Handler (FormResult Device, Widget)
deviceForm userIdMaybe users = renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 3)) $ Device
  <$> areq (selectFieldList [(userName user, userid) | Entity userid user <- users ]) (bfs ("持ち主" :: Text)) userIdMaybe
  <*> areq textField (bfs ("MACアドレス" :: Text)) Nothing
  <*  bootstrapSubmit (BootstrapSubmit ("登録" :: Text) "btn-primary" [("attribute-name","attribute-value")])
