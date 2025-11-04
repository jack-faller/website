#!/bin/sh
MAX_LINE=$(wc -l < "$1")
SCRIPT='{printf " %'$(echo -n "$MAX_LINE" | wc -c)'s  %s\n", NR, $0}'
echo $SCRIPT > /dev/stderr
tree-sitter highlight --config-path build/ts-config.json "$1" | awk "$SCRIPT" | aha --no-header --no-xml | sed 's/#d5d5d5/black/g'
