#!/bin/sh
# Fetch the logs from the server
set -ex
DIR="$1"
if [ -z "$DIR" ]; then
    DIR="$(mktemp -d /tmp/getlog.XXXXX)"
fi
rsync --recursive --include='access.log*' --exclude='*' "$(cat "$(dirname "$(which "$0")")"/server-url.txt)":/var/log/nginx/ "$DIR"
