#!/usr/bin/env sh

setxkbmap -query | grep layout | awk '{print $2}'

