#!/usr/bin/env sh

curl -s "wttr.in/Cairo?format=3" | awk '{print $3}'
