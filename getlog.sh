#!/bin/sh
# Print the access logs from the server.
set -ex
cd "$(mktemp -d /tmp/getlog.XXXXX)"
rsync --recursive --include='access.log*' --exclude='*' "$(cat "$(dirname "$(which "$0")")"/server-url.txt)":/var/log/nginx/ ./
gunzip *.gz
cat *
