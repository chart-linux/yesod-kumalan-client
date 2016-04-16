module Handler.UserPay where

import Import

getUserPayR :: UserId -> Handler Html
getUserPayR userId = do
  user <- runDB $ get404 userId
  paidMaybe <- lookupGetParam "paid"
  case (readMay =<< paidMaybe) of
    Just paid -> runDB $ update userId [UserPaid =. paid]
    Nothing -> return ()
  redirect (UsersR, [("place_id", pack . show $ userPlaceId user)])
