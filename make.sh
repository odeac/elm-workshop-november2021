args="--output elm-app.js src/Main.elm"

case $1 in
  "live") elm-live -- $args ;;
  "build") elm make $args ;;
esac