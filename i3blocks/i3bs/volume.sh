#!/usr/bin/env sh

amixer get Master | sed -n '/^\s*Front Left/p' | awk '{print $5}' | sed 's/[][]//g'
