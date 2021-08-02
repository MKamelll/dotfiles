#!/bin/bash

# Colors
lightCyan="\033[1;36m"
lightRed="\033[1;31m"
noColor="\033[0m"

# Check for root
[[ $EUID > 0 ]] && echo "${lightRed}The script needs to be ran as root${noColor}"

# Cloning
echo "${lightCyan}Cloning in ${PWD}${noColor}"
git clone --recurse-submodules -j8 git://github.com/Stremio/stremio-shell.git

# Install dependencies
echo "${lightCyan}Installing dependencies..${noColor}"
zypper install libqt5-creator mpv-devel libcaca-devel ncurses5-devel libQt5WebView5 libSDL2-devel qconf messagelib-devel libqt5-qtwebengine-devel libopenssl-devel rpmdevtools nodejs8 libQt5WebChannel5-imports libqt5-qtwebengine libQt5QuickControls2-5 libqt5-qtquickcontrols libqt5-qtquickcontrols2

# Resolve needed packages
zypper search -i rsvg-conver || zypper install rsvg-conver
zypper search -i ffmpeg-4    || zypper install ffmpeg-4

# Build
cd stremio-shell
make -Bf release.makefile

# Install
make -f release.makefile install

# For running as a command
ln -s /opt/stremio/stremio /usr/local/bin/stremio

# Add the icon to dash
ln -s /opt/stremio/smartcode-stremio.desktop ~/.local/share/applications/smartcode-stremio.desktop
