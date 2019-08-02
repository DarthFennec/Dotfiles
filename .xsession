#!/bin/bash
export PATH=/home/tucker/bin:/home/tucker/.local/bin:$PATH
export LOGOUT_COMMAND=xlogout
export BROWSER=chromium
export QT_STYLE_OVERRIDE='gtk2'
xrandr --output HDMI-1 --mode 1920x1080 -r 120
[ -s ~/.Xmodmap ] && xmodmap ~/.Xmodmap
xset s off
xset -dpms
xrdb ~/.Xresources
xsetroot -cursor_name left_ptr
wmname LG3D
trayer --edge top --align right --margin 0 --padding 0 --width 100 \
--height 25 --widthtype pixel --transparent true --alpha 0 --tint 0x000000 &
mpd
dunst &
xmoconk &
xbanish &
discord &
sudo /sbin/iptables -I INPUT -p tcp -m tcp --dport 3000 -j ACCEPT
