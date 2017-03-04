#!/bin/bash
export PATH=/home/tucker/bin:$PATH
export LOGOUT_COMMAND=xlogout
export BROWSER=chromium
export QT_STYLE_OVERRIDE='gtk2'
export QEMU_AUDIO_DRV='pa'
export QEMU_PA_SERVER='/run/user/1000/pulse/native'
export QEMU_PA_SINK='alsa_output.pci-0000_00_1b.0.analog-stereo'
export QEMU_PA_SOURCE='input'
[ -s ~/.Xmodmap ] && xmodmap ~/.Xmodmap
xrandr --output VGA1 --mode 1920x1080
xrandr --output HDMI1 --mode 1920x1080 --below VGA1
xset s off
xset -dpms
xrdb ~/.Xresources
xsetroot -cursor_name left_ptr
trayer --edge top --align right --margin 0 --padding 0 --width 100 \
--height 25 --widthtype pixel --transparent true --alpha 0 --tint 0x000000 &
synergys --address 192.168.122.1 --config ~/synergy.conf --restart --daemon
mpd
conky | xmoconk &
xbanish &
dropbox &
hexchat &
exec xmonad
