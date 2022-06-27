#!/bin/bash
URL="./provskell.html"
SERVER="./server.js"
[[ -x $BROWSER ]] && exec "$BROWSER" "$URL"
path=$(which xdg-open || which gnome-open) && exec "$path" "$URL" &
exec node "$SERVER"
echo "Can't find browser"
