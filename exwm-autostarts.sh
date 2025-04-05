#!/bin/bash
# ~/.config/exwm-autostarts.sh

# Ensure X environment is set
export DISPLAY=${DISPLAY:-:0}

# Log to a file for debugging
logfile="$HOME/.exwm-autostart.log"
echo "$(date): Starting EXWM autostart" > "$logfile"

# GUI Applets
if command -v nm-applet >/dev/null 2>&1; then
    nm-applet & disown
    echo "$(date): Launched nm-applet" >> "$logfile"
else
    echo "$(date): nm-applet not found" >> "$logfile"
fi

if command -v blueman-applet >/dev/null 2>&1; then
    blueman-applet & disown
    echo "$(date): Launched blueman-applet" >> "$logfile"
else
    echo "$(date): blueman-applet not found" >> "$logfile"
fi

if command -v udiskie >/dev/null 2>&1; then
    udiskie -as 2>>"$logfile" &
    disown
    echo "$(date): Launched udiskie" >> "$logfile"
else
    echo "$(date): udiskie not found" >> "$logfile"
fi

# Power Management
if command -v xss-lock >/dev/null 2>&1 && command -v slock >/dev/null 2>&1; then
    xss-lock --transfer-sleep-lock -- slock & disown
    echo "$(date): Launched xss-lock" >> "$logfile"
else
    echo "$(date): xss-lock or slock not found" >> "$logfile"
fi

if command -v xautolock >/dev/null 2>&1 && command -v slock >/dev/null 2>&1; then
    xautolock -time 10 -locker 'slock' & disown
    echo "$(date): Launched xautolock" >> "$logfile"
else
    echo "$(date): xautolock or slock not found" >> "$logfile"
fi

echo "$(date): Autostart completed" >> "$logfile"
