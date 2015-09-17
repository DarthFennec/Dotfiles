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

# Bind my vim escape sequence
if [ "$TERM" != "eterm-color" ]
then
bindkey -M viins 'jk' vi-cmd-mode
fi
bindkey "^?" backward-delete-char

# Custom prompt
autoload -U promptinit && promptinit
PROMPT="[%1~]: "

# Colored output for less
man() {
env LESS_TERMCAP_mb=$'\E[01;31m' \
LESS_TERMCAP_md=$'\E[01;38;5;74m' \
LESS_TERMCAP_me=$'\E[0m' \
LESS_TERMCAP_se=$'\E[0m' \
LESS_TERMCAP_so=$'\E[38;5;246m' \
LESS_TERMCAP_ue=$'\E[0m' \
LESS_TERMCAP_us=$'\E[04;38;5;146m' \
man "$@"
}

# Colored output for ls and grep
eval $(dircolors -b)
alias ls="ls -A --color=auto"
alias grep="grep --color=auto"

# Emacs location hinting
if [ "$TERM" = "eterm-color" ]
then
precmd() {
echo -e "\eAnSiTu" "$LOGNAME"
echo -e "\eAnSiTc" "$(pwd)"
echo -e "\eAnSiTh" "$(hostname -f)"
}
fi

# Emacs edit hinting
if [ "$TERM" = "eterm-color" ]
then
export ETERM=eterm
alias e='echo -e "\eAnSiTe"'
alias x='echo -e "\eAnSiTx"'
else
alias e=vim
alias x=vim
fi

# Miscellaneous
alias cls="echo -ne '\ec'"
preexec() { d="$(echo "$3" | sed "s:^~:$HOME:")" ; [ -d $d ] && ls $d }
export CHROMIUM_USER_FLAGS="--enable-print-preview"
export JAVA_HOME="$(readlink -f /usr/bin/javac | sed "s:bin/javac::")"
export _JAVA_AWT_WM_NONREPARENTING=1
export LOGOUT_COMMAND=/home/tucker/bin/xlogout
export TERM=xterm-256color
export EDITOR=vim
export VISUAL=vim
