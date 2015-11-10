HISTFILE=~/.zsh_history
HISTSIZE=500
SAVEHIST=500
setopt appendhistory autocd extendedglob notify
unsetopt beep nomatch
if [ -z "$INSIDE_EMACS" ]
then
    bindkey -v 'jk' vi-cmd-mode
    bindkey '^?' backward-delete-char
fi

zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit

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

# Jump to position if possible
function try-jump-start-pos { TRY_JUMP_PARAM=""  ; zle -K try-jump-map }
function try-jump-start-neg { TRY_JUMP_PARAM="-" ; zle -K try-jump-map }
function try-jump-param { TRY_JUMP_PARAM="$TRY_JUMP_PARAM$KEYS" }
function try-jump {
    TRY_JUMP_PARAM=$((TRY_JUMP_PARAM+CURSOR))
    if [ $TRY_JUMP_PARAM -ge 0 -a $TRY_JUMP_PARAM -le ${#BUFFER} ]
    then
        CURSOR=$TRY_JUMP_PARAM
    fi
    unset TRY_JUMP_PARAM
    zle -K main
}
zle -N try-jump-start-pos
zle -N try-jump-start-neg
zle -N try-jump-param
zle -N try-jump
bindkey -N try-jump-map
bindkey -M try-jump-map -R '0-9' try-jump-param
bindkey -M try-jump-map 'x' try-jump
bindkey '^[[j:' try-jump-start-pos
bindkey '^[[j:-' try-jump-start-neg

# Emacs location hinting
if [ ! -z "$INSIDE_EMACS" ]
then
    precmd() {
        echo -e "\eAnSiTu" "$LOGNAME"
        echo -e "\eAnSiTc" "$(pwd)"
        echo -e "\eAnSiTh" "$(hostname -f)"
    }
fi

# Emacs edit hinting
if [ ! -z "$INSIDE_EMACS" ]
then
    alias e='echo -e "\eAnSiTe"'
    alias x='echo -e "\eAnSiTx"'
    alias man='echo -e "\eAnSiTm"'
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
export LOGOUT_COMMAND=xlogout
export BROWSER='chromium-browser'
export TERM=xterm-256color
export GOPATH=$HOME/go
export EDITOR=vim
export VISUAL=vim
