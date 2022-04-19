#!/usr/bin/env sh

increase_volume() {
    amixer -q sset Master "3%+"
}

decrease_volume() {
    amixer -q sset Master "3%-"
}

mute() {
    amixer -q sset Master mute
}

unmute() {
    amixer -q sset Master unmute
}

update() {
    local value=$(amixer get Master | sed -n '/^\s*Front Left/p' | awk '{print $5}' | sed 's/[][]//g')

    local status=$(amixer get Master | sed -n '/^\s*Front Left/p' | awk '{print $6}' | sed 's/[][]//g')

    if [[ "$status" == "off" ]]; then
        echo "muted"
        return
    fi

    echo "$value"
}

if   [[ "$button" == "1" ]]; then
    mute
elif [[ "$button" == "3" ]]; then
    unmute
elif [[ "$button" == "4" ]]; then
    increase_volume
elif [[ "$button" == "5" ]]; then
    decrease_volume
fi

update
