module Handler.Firewall where

import Import hiding (writeFile)
import System.IO
import System.Process

sshTarget = "root@kumalan.mydns.jp"

getFirewallR :: Handler Html
getFirewallR = do
  users <- runDB $ selectList ([ UserPaid ==. True ] ||. [ UserExempted ==. True ]) []
  _devices <- forM users $ \(Entity userid _) -> do
    runDB $ selectList [ DeviceUserId ==. userid ] []
  let devices = concat _devices
  liftIO $ do
    accepts <- forM devices $ \(Entity _ device) -> do
      let mac = map (\c -> if c == '-' then ':' else c) (unpack $ deviceMacAddress device)
      return ("iptables -A forwarding_lan_rule -m mac --mac-source " ++ mac ++ " -j ACCEPT")
    let script = accepts ++ ["iptables -A forwarding_lan_rule -j REJECT"]
    writeFile "/tmp/firewall.user" (unlines script)
    callProcess "/usr/bin/scp" ["-o", "StrictHostKeyChecking=no", "-o","UserKnownHostsFile=/dev/null", "/tmp/firewall.user", sshTarget ++ ":/etc/firewall.user"]
    callProcess "/usr/bin/ssh" ["-o", "StrictHostKeyChecking=no", "-o", "UserKnownHostsFile=/dev/null", sshTarget, "/etc/init.d/firewall", "restart"]
  redirect HomeR
