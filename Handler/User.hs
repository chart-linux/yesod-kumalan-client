module Handler.User where

import Import

getUsersR :: Handler Html
getUsersR = do
  placeIdMaybe <- lookupGetParam "place_id"
  case (readMay =<< placeIdMaybe) of
    Just placeId -> do
      place <- runDB $ get404 placeId
      users <- runDB $ selectList [ UserPlaceId ==. placeId ] []
      defaultLayout $(widgetFile "user/index")
    Nothing -> notFound

getUserCreateR :: Handler Html
getUserCreateR = do
  placeIdMaybe <- lookupGetParam "place_id"
  places <- runDB $ selectList [] []
  (formWidget, formEnctype) <- generateFormPost $ userForm (readMay =<< placeIdMaybe) places
  defaultLayout $(widgetFile "user/create")

postUserCreateR :: Handler Html
postUserCreateR = do
  places <- runDB $ selectList [] []
  ((result, formWidget), formEnctype) <- runFormPost $ userForm Nothing places
  case result of
    FormSuccess user -> do
      _ <- runDB $ insert user
      redirect (UsersR, [("place_id", (pack . show $ userPlaceId user))])
    _ -> do
      defaultLayout $(widgetFile "user/create")

getUserEditR :: UserId -> Handler Html
getUserEditR userId = do
  user <- runDB $ get404 userId
  places <- runDB $ selectList [] []
  (formWidget, formEnctype) <- generateFormPost $ updateUserForm (Just user) Nothing places
  defaultLayout $(widgetFile "user/edit")

postUserEditR :: UserId -> Handler Html
postUserEditR userId = do
  places <- runDB $ selectList [] []
  ((result, formWidget), formEnctype) <- runFormPost $ updateUserForm Nothing Nothing places
  case result of
    FormSuccess user -> do
      _ <- runDB $ get404 userId
      _ <- runDB $ repsert userId user
      redirect (UsersR, [("place_id", pack . show $ userPlaceId user)])
    _ -> defaultLayout $(widgetFile "user/edit")

getUserDeleteR :: UserId -> Handler Html
getUserDeleteR userId = do
  user <- runDB $ get404 userId
  _ <- runDB $ delete userId
  redirect (UsersR, [("place_id", pack . show $ userPlaceId user)])

updateUserForm :: Maybe User -> Maybe PlaceId -> [Entity Place] -> Html -> MForm Handler (FormResult User, Widget)
updateUserForm user placeIdMaybe places = renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 1) (ColSm 0) (ColSm 3)) $ User
  <$> areq (selectFieldList [(placeName place, placeid) | Entity placeid place <- places ]) (bfs ("部屋" :: Text)) (Just . userPlaceId =<< user)
  <*> areq textField (bfs ("名前" :: Text)) (Just . userName =<< user)
  <*> areq boolField (bfs ("免除対象?" :: Text)) (Just . userExempted =<< user)
  <*> areq boolField (bfs ("支払い済み?" :: Text)) (Just . userPaid =<< user)
  <*  bootstrapSubmit (BootstrapSubmit ("登録" :: Text) "btn-primary" [("attribute-name","attribute-value")])

userForm :: Maybe PlaceId -> [Entity Place] -> Html -> MForm Handler (FormResult User, Widget)
userForm = updateUserForm Nothing
