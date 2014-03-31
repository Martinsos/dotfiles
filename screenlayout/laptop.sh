#!/bin/sh
# Use only laptop screen
xrandr --output HDMI1 --off \
       --output VGA1 --off \
       --output VIRTUAL1 --off \
       --output DP1 --off
xrandr --output LVDS1 --mode 1366x768 --pos 0x0 --rotate normal
