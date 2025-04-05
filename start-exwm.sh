#!/bin/sh

# Allow local user to access X server
xhost +SI:localuser:$USER

# Export environment variables
export _JAVA_AWT_WM_NONREPARENTING=1
export XDG_SESSION_TYPE=x11

# Set cursor and keyboard rate
xsetroot -cursor_name left_ptr
xset r rate 200 60

# Source user’s X session setup (if any)
[ -f ~/.xprofile ] && . ~/.xprofile

# Ensure Emacs is in the PATH
if ! command -v emacs >/dev/null 2>&1; then
    echo "Emacs not found. Please install it."
    exit 1
fi

# Start Emacs with EXWM, logging output
exec emacs --eval "(shell-command \"~/.config/emacs/exwm-autostarts.sh &\")"
