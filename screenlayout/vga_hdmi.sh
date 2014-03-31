#!/bin/sh
# Use two displays, VGA and HDMI, where VGA is left of HDMI
xrandr --output LVDS1    --off \
       --output VIRTUAL1 --off \
       --output DP1      --off
xrandr --output VGA1  --mode 1920x1080 --pos 0x0    --rotate normal \
       --output HDMI1 --mode 1920x1080 --pos 1920x0 --rotate normal
