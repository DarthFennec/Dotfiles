HISTFILE=~/.zsh_history
HISTSIZE=500
SAVEHIST=500
setopt incappendhistory autocd extendedglob notify bashrematch globdots
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
PROMPT="[%1~]: "

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

# Better syntax highlighting
source \
    /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Jump to position if possible
function try-run-command {
    BLEN="${#BUFFER}"
    read -sk ; AS="$REPLY"
    while [[ "$REPLY" != ':' ]] ; do read -sk ; AS="$AS$REPLY" ; done
    read -sk '3' ; AS="$AS$REPLY"
    if [[ "$AS" =~ '^(-?[0-9]+)(,(-?[0-9]+))?(#([0-9]+))?(;([0-9]+))?:(...)$' ]]
    then
        PT="${BASH_REMATCH[2]}"     # Point Location
        MK="${BASH_REMATCH[4]}"     # Mark Location
        CT="${BASH_REMATCH[6]}"     # Numeric Argument
        PS="${BASH_REMATCH[8]}"     # Paste Data Size
        OP="${BASH_REMATCH[9]:0:1}" # Operator
        ST="${BASH_REMATCH[9]:1:1}" # Selection Type
        YT="${BASH_REMATCH[9]:2:1}" # Yank Selection Type
        [[ -z "$MK" ]] && MK="$PT"
        [[ -z "$CT" ]] && CT='1'
        [[ -z "$PS" ]] && PS='0'
        [[ "$PS" -gt '0' ]] && { read -sk "$PS" ; PD="$REPLY" ; }
        PT="$((PT+CURSOR))"
        MK="$((MK+CURSOR))"
        if [[ "$OP$YT" =~ 'a.|pc' ]] ; then PT="$((PT+1))"
        elif [[ "$OP$ST$YT" =~ '[AI]..|[pP].l|.l.' ]] ; then
            VECT='1' ; [[ "$OP$ST" =~ '[IP].|.l' ]] && VECT='-1'
            while [[ "${BUFFER:$PT:1}" != $'\n' && \
                           "$PT" -ge '0' && "$PT" -lt "$BLEN" ]]
            do PT="$((PT+VECT))" ; done
            [[ "$OP" =~ '[IPcp]' ]] && PT="$((PT+1))"
            unset VECT
        fi
        if [[ "$ST" == 'l' ]] ; then
            MK="$((MK-1))"
            while [[ "${BUFFER:$MK:1}" != $'\n' && \
                           "$MK" -ge '0' && "$MK" -lt "$BLEN" ]]
            do MK="$((MK+1))" ; done
        fi
        if [[ "$OP" =~ '[aiAI]' && "$PT" -ge '0' ]] ; then CURSOR="$PT"
        elif [[ "$OP" =~ '[pP]' && "$PT" -ge '0' ]] ; then
            PDS=''
            for i in {1.."$CT"} ; do PDS="$PDS$PD" ; done
            BUFFER="${BUFFER:0:$PT}$PDS${BUFFER:$PT}"
            CURSOR="$((PT+CT*PS-1))"
            unset PDS
        elif [[ "$OP" =~ '[cdCD]' ]] ; then
            [[ "$PT" -lt '0' ]] && PT='0'
            [[ "$MK" -gt "$BLEN" ]] && MK="$BLEN"
            if [[ "$PT" -lt "$MK" ]] ; then
                BUFFER="${BUFFER:0:$PT}${BUFFER:$MK}"
                CURSOR="$PT"
            fi
        fi
        unset PT MK CT PS OP ST YT PD
    fi
    unset BLEN AS
}
zle -N try-run-command
bindkey '^[[::' try-run-command

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
preexec() {
    d="$(sed "s:^~:$HOME:" <<<"$3")"
    [ -d $d ] && ! type $d >/dev/null 2>&1 && ls $d
}
export CHROMIUM_FLAGS="--enable-print-preview"
export JAVA_HOME="$(readlink -f /usr/bin/javac | sed "s:bin/javac::")"
export _JAVA_AWT_WM_NONREPARENTING=1
export TERM=xterm-256color
export GOPATH=$HOME/go
export EDITOR=vim
export VISUAL=vim
