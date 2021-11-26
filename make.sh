args="--output elm-app.js --debug src/Main.elm"

case $1 in
  "live") elm-live --proxyHost http://totallysafelink.xyz --proxyPrefix /api --verbose -- $args ;;
  "build") elm make $args ;;
esac