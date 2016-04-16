module Handler.Place where

import Import

getPlacesR :: Handler Html
getPlacesR = do
  places <- runDB $ selectList [] []
  defaultLayout $(widgetFile "place/index")

getPlaceCreateR :: Handler Html
getPlaceCreateR = do
  (formWidget, formEnctype) <- generateFormPost placeForm
  defaultLayout $(widgetFile "place/create")

postPlaceCreateR :: Handler Html
postPlaceCreateR = do
  ((result, formWidget), formEnctype) <- runFormPost placeForm
  case result of
    FormSuccess place -> do
      _ <- runDB $ insert place
      redirect PlacesR
    _ -> defaultLayout $(widgetFile "place/create")

getPlaceEditR :: PlaceId -> Handler Html
getPlaceEditR placeId = error "Not yet implemented: getPlaceEditR"

postPlaceEditR :: PlaceId -> Handler Html
postPlaceEditR placeId = error "Not yet implemented: postPlaceEditR"

getPlaceDeleteR :: PlaceId -> Handler Html
getPlaceDeleteR placeId = do
  place <- runDB $ get404 placeId
  _ <- runDB $ delete placeId
  redirect HomeR

placeForm :: Html -> MForm Handler (FormResult Place, Widget)
placeForm = renderBootstrap3 BootstrapBasicForm $ Place
  <$> areq textField (bfs ("名前" :: Text)) Nothing
