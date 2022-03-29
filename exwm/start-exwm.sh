#!/bin/sh
#!/bin/bash
# Very important: Avoid spawning daemons here.
# They will not exit with this process, so we will no longer have a clean X11 shutdown.

xset -dpms
xset s off

killall emacs
# Disable access control for the current user.
xhost +SI:localuser:$USER

## you might need to append the TTY you are working on
# xinit

wmname LG3D

# ## Run site init scripts. Usually not necessary.
# if [ -d /etc/X11/xinit/xinitrc.d ] ; then
#     for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
#         [ -x "$f" ] && . "$f"
#     done
#     unset f
# fi

# . ~/exwm_screen_layout
# ~/exwm_xrandr.bash

# Set themes, etc.
# xrdb -override ~/exwm_x11_resources.xrdb
# Note: xsettingsd just publishes settings. You must ensure that it has settings to publish.
# /usr/bin/xsettingsd &
# Try to control screen blanking
# xset s off dpms 1200 1400 1600
# Set keyboard repeat rate. Default is 660 25 ("xset q")
# xset r rate 200 30

# Set default cursor.
xsetroot -cursor_name left_ptr

# Hide the mouse pointer if unused for a duration
# /usr/bin/unclutter &

# unclutter --jitter 3 --ignore-scrolling &
# One can also start processes unrelated to X11, just ensure that they will exit when this process exits.

# Enable "Num Lock" mode, on keyboard keypad
# /usr/bin/numlockx on &

  # Run the screen compositor
# picom &

  # xsettingsd_preset_file="${XDG_DATA_HOME:-$HOME/.local/share}/xsettingsd/presets/dark"
  # xsettingsd_config_file="${XDG_CONFIG_HOME:-$HOME/.config}/xsettingsd/xsettingsd"

  # ln -sf "$xsettingsd_preset_file" "$xsettingsd_config_file" \
  #     && xsettingsd -c "$xsettingsd_config_file" &

eval $(gnome-keyring-daemon -s)
export SSH_AUTH_SOCK
  # Enable screen locking on suspend
xss-lock -- slock &
# Uncomment the following block to use the exwm-xim module. Not something I use.
export XMODIFIERS=@im=exwm-xim
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export CLUTTER_IM_MODULE=xim

# If Emacs is started in server mode, `emacsclient` is a convenient way to
# edit files in place (used by e.g. `git commit`).

export VISUAL=emacsclient
export EDITOR="$VISUAL"
# Finally start Emacs
# Scrolling gtk3 apps won't work, unless GDK_CORE_DEVICE_EVENTS is defined
export GDK_CORE_DEVICE_EVENTS=1
# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# exec dbus-launch --exit-with-session /usr/local/bin/emacs --eval "(progn (require 'exwm) (exwm-enable))"
# "exwm-enable" has to be called before the frame is spawned.
# usr/local/bin/emacs --daemon --eval "(require 'exwm)" -f exwm-enable
# emacs --daemon --eval "(require 'exwm)" -f exwm-enable
# exec dbus-launch --exit-with-session emacs -mm #--debug-init

# exec emacs --daemon && emacsclient -c --eval "(exwm-enable)"
exec dbus-launch --exit-with-session emacs --eval "(exwm-enable)"
# exec emacsclient -c
