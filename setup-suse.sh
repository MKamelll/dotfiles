#!/bin/bash

# Colors
lightCyan="\033[1;36m"
lightRed="\033[1;31m"
noColor="\033[0m"

# Check if running as root
[[ $EUID > 0 ]] && echo "The script must be run as root"

# Use the synaptics driver for touchpad
zypper search -i "synaptics" || zypper install "xf86-input-synaptics"

ln -s usr/share/X11/xorg.conf.d/70-synaptics.conf /etc/X11/70-synaptics.conf
ln -s usr/share/X11/xorg.conf.d/40-libinput.conf /etc/X11/40-libinput.conf

echo "${lightCyan}Don't forget to comment out the touchpad section in libinput.conf${noColor}"

# Stop gnome-software from autostart
[ ! -d ~/.config/autostart ] && mkdir ~/.config/autostart

cp /etc/xdg/autostart/gnome-software-service.desktop  .config/autostart/

echo "X-GNOME-Autostart-enabled=false" >> .config/autostart/gnome-software-service.desktop

# TODO: Add installing nvidia here
# TODO: Add installing suse-prime-bbswitch here
# TODO: Prompt for the initial prime-select profile

# Force gdm to use X to use the touchpad on login
echo "${lightCyan}Uncomment WaylandEnabled=false in /etc/gdm/custom.conf"

# Set display for gdm to be the primary always
cp .config/monitors.xml /var/lib/gdm/.config
