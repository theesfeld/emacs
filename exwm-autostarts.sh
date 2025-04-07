#!/bin/bash
# ~/.config/exwm-autostarts.sh

# Ensure X environment is set
export DISPLAY=${DISPLAY:-:0}
export XAUTHORITY=${XAUTHORITY:-$HOME/.Xauthority}

# Log to a file with PID tracking
logfile="$HOME/.exwm-autostart.log"
echo "$(date): Starting EXWM autostart (PID $$)" > "$logfile"
echo "$(date): DISPLAY=$DISPLAY, XAUTHORITY=$XAUTHORITY" >> "$logfile"

# Function to launch and verify
launch_app() {
    local cmd="$1"
    if command -v "$cmd" >/dev/null 2>&1; then
        "$cmd" & disown
        local pid=$!
        echo "$(date): Launched $cmd (PID $pid)" >> "$logfile"
        sleep 1  # Give it a moment to start
        if ps -p "$pid" >/dev/null 2>&1; then
            echo "$(date): $cmd (PID $pid) is running" >> "$logfile"
        else
            echo "$(date): $cmd (PID $pid) failed to persist" >> "$logfile"
        fi
    else
        echo "$(date): $cmd not found" >> "$logfile"
    fi
}

# GUI Applets
launch_app "nm-applet"
launch_app "blueman-applet"
launch_app "udiskie -as"

# Power Management
if command -v xss-lock >/dev/null 2>&1 && command -v slock >/dev/null 2>&1; then
    xss-lock --transfer-sleep-lock -- slock & disown
    pid=$!
    echo "$(date): Launched xss-lock (PID $pid)" >> "$logfile"
    sleep 1
    if ps -p "$pid" >/dev/null 2>&1; then
        echo "$(date): xss-lock (PID $pid) is running" >> "$logfile"
    else
        echo "$(date): xss-lock (PID $pid) failed to persist" >> "$logfile"
    fi
else
    echo "$(date): xss-lock or slock not found" >> "$logfile"
fi

if command -v xautolock >/dev/null 2>&1 && command -v slock >/dev/null 2>&1; then
    xautolock -time 10 -locker 'slock' & disown
    pid=$!
    echo "$(date): Launched xautolock (PID $pid)" >> "$logfile"
    sleep 1
    if ps -p "$pid" >/dev/null 2>&1; then
        echo "$(date): xautolock (PID $pid) is running" >> "$logfile"
    else
        echo "$(date): xautolock (PID $pid) failed to persist" >> "$logfile"
    fi
else
    echo "$(date): xautolock or slock not found" >> "$logfile"
fi

echo "$(date): Autostart completed" >> "$logfile"
