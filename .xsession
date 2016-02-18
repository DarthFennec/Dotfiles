#!/bin/bash
export PATH=/home/tucker/bin:/home/tucker/.cabal/bin:$PATH
export LOGOUT_COMMAND=xlogout
export BROWSER=chromium
[ -s ~/.Xmodmap ] && xmodmap ~/.Xmodmap
xrandr --output DVI-I-1 --mode 1920x1080
xrandr --output HDMI-0 --mode 1920x1080 --below DVI-I-1
xset s off
xset -dpms
xrdb ~/.Xresources
xsetroot -cursor_name left_ptr
pulseaudio --start --log-target=syslog
mpd
xbanish &
pavucontrol &
conky | xmoconk &
trayer --edge top --align right --margin 0 --padding 0 --width 100 \
--height 25 --widthtype pixel --transparent true --alpha 0 --tint 0x000000 &
dropbox &
exec xmonad
