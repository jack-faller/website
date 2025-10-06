#!/bin/sh
export THISDIR=$(dirname "$(which "$0")")
guile -L "$THISDIR" -L "$THISDIR/libraries/scheme-utilities" $@
