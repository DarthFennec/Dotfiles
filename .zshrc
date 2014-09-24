# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=500
SAVEHIST=500
setopt appendhistory autocd extendedglob notify
unsetopt beep nomatch
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/tucker/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
autoload -U promptinit && promptinit
PROMPT="[%1~]: "
alias cls="echo -ne '\ec'"
alias ls="ls -A --color=auto"
export CHROMIUM_USER_FLAGS="--enable-print-preview"
export _JAVA_AWT_WM_NONREPARENTING=1
export EDITOR=/usr/bin/vim
export LOGOUT_COMMAND=/home/tucker/bin/xlogout
export TERM=xterm-256color
export KEYTIMEOUT=1
