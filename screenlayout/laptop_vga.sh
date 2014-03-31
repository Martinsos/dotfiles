#!/bin/sh
# Use laptop as left and VGA as right display
xrandr --output HDMI1 --off \
       --output VIRTUAL1 --off \
       --output DP1 --off
xrandr --output LVDS1 --mode 1366x768  --pos 0x0    --rotate normal \
       --output VGA1  --mode 1920x1080 --pos 1366x0 --rotate normal
