#!/bin/bash
# Fix for Intel Xe driver monitor configuration with EXWM

# First, ensure all monitors are detected properly
xrandr --auto
sleep 0.5

# Set each monitor individually with a small delay
xrandr --output DP-3-1-6 --mode 3840x2160 --pos 0x0 --primary
sleep 0.2
xrandr --output DP-3-2 --mode 3840x2160 --pos 3840x0
sleep 0.2
xrandr --output eDP-1 --mode 2880x1800 --scale 0.67x0.67 --pos 7680x0 --brightness 1.0

# Verify the configuration
echo "Monitor configuration set. Current status:"
xrandr --current | grep " connected"