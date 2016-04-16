stack build
if [ -e devel.pid ] ; then
  kill $(cat devel.pid)
fi
stack exec yesod-kumalan &
echo $! > devel.pid
