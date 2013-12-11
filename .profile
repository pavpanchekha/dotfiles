#!/bin/sh

# Bash customizations
CDPATH=".:~" # `cd _` can equal `cd ~/_`
HISTIGNORE="&:bg:fg:exit: *" # Ignore duplicate commands, bg, fg, exit
HISTFILE=".cache/history.bash"
export PS1 CDPATH HISTIGNORE HISTFILE

if [ "x$EUID" = "x0" ]; then
    export TMOUT=60
    PS1="\[\033[1;31m\]\w\[\033[0m\]:\$> "
else
    PS1="\[\033[0;36m\]\w\[\033[0m\]:\$> "
fi

if [ "x$TERM" = "xxterm" ]; then
    PROMPT_COMMAND='printf "\033]0;Bash: %s@%s %s\007" "${USER}" "${HOSTNAME}" "${PWD/#$HOME/~}"'
else
    PROMPT_COMMAND=''
fi

shopt -s cdspell # Spell-correct `cd _`
shopt -s cmdhist # Add multiline commands as one-liners in my history
shopt -s extglob # Crazy globbing tricks: [?*+@!]\(xxxx\)

# Completion
. /usr/share/bash-completion/bash_completion &

# Explain commands that weren't found
source /usr/share/doc/pkgfile/command-not-found.bash

# Some actual environment variables
PATH="$HOME/bin:$PATH"
GPGKEY="180C02EB09D0BE15"
LESSHISTFILE="/dev/null"
export PATH TEXINPUTS GPGKEY LESSHISTFILE
umask 077
eval `dircolors -b`

# Some definitions of programs
 EDITOR="vim"
  PAGER="less"
BROWSER="w3m"
export EDITOR PAGER BROWSER

# Over SSH, Mathematica fonts need setting
if [ -d /usr/local/mathematica/fonts/Type1 ]; then
    xset fp+ /usr/local/mathematica/fonts/Type1
    xset fp+ /usr/local/mathematica/fonts/BDF
fi

# Utility functions
function project () {
    local output mode
    output=`xrandr | grep connected | cut -d\  -f1 | grep -v LVDS1 | head -n1`
    mode=`xrandr | grep "$output" -a1 | tail -n1 | cut -f4 -d\ `
    xrandr --output "$output" --mode "$mode"
}

function drop () {
    scp "$1" server:~www/drop/"$2" 1>&2
    if [ -z "$2" ]; then
        name="`basename $1`"
    else
        name="$1"
    fi
    ssh server "cd ~www/drop && chmod a+r $name"
    
    echo "http://drop.pavpanchekha.com/$name"
}

function search () {
    tracker-search "$@" | tail -n +2 | head -n -1 | cut -c10-
}

function to-emacs () {
    bg
    emacsclient -c -e '(term-simple-send (get-buffer-process (ansi-term "/bin/bash")) "reptyr '`jobs -p %+`'")'
    kill $$
}

alias math="rlwrap math"
alias sbcl="rlwrap sbcl"
alias scheme="rlwrap scheme"
alias zephyr="tmux -L zephyr attach"
alias edconfigure="$EDITOR ~/.profile"
alias reconfigure=". ~/.profile"
alias py="ipython"

# Switch to using non-destructive trashing when possible.
alias rm="rm -i"
alias tm="trash"

