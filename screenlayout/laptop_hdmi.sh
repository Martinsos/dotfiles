#!/bin/sh
# Laptop as left screen, HDMI as right screen
xrandr --output VIRTUAL1 --off \
       --output DP1      --off \
       --output VGA1     --off
xrandr --output HDMI1 --mode 1920x1080 --pos 1366x0 --rotate normal \
       --output LVDS1 --mode 1366x768  --pos 0x0    --rotate normal
