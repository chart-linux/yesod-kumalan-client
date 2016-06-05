module Handler.Firewall where

import Import hiding (hPutStrLn)
import System.IO.Temp
import System.Process
import System.IO

getFirewallR :: Handler Html
getFirewallR = do
  users <- runDB $ selectList [ UserPaid ==. True ] []
  _devices <- forM users $ \(Entity userid _) -> do
    runDB $ selectList [ DeviceUserId ==. userid ] []
  let devices = concat _devices
  withSystemTempFile "firewall.user" $ \path hdl -> liftIO $ do
    forM_ devices $ \(Entity _ device) -> do
      let mac = map (\c -> if c == '-' then ':' else c) (unpack $ deviceMacAddress device)
      hPutStrLn hdl ( "iptables -A forwarding_lan_rule -m mac --mac-source " ++ mac ++ " -j ACCEPT" )
    hFlush hdl
    callCommand ("scp -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null " ++ path ++ " root@kumalan.dip.jp:/etc/firewall.user")
    callCommand "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@kumalan.dip.jp /etc/init.d/firewall restart"
  redirect HomeR
