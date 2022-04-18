#!/usr/bin/env sh

uptime | awk '{print $8}' | sed 's/,//g'
