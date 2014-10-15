#!/bin/bash
export PATH=.:/home/tucker/bin:/home/tucker/.cabal/bin:$PATH
xrdb ~/.Xresources
xsetroot -cursor_name left_ptr
pulseaudio --start --log-target=syslog
mpd
pavucontrol &
conky | xmoconk &
trayer --edge top --align right --margin 0 --padding 0 --width 100 \
--height 25 --widthtype pixel --transparent true --alpha 0 --tint 0x000000 &
[[ ! -e /usr/bin/weechat ]] || roxterm -e weechat &
dropboxd &
exec xmonad
