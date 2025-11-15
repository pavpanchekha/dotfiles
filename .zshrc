#!/bin/zsh

# Load colors for prompt
autoload -U colors && colors

if [ "x$EUID" = "x0" ]; then
    export TMOUT=60
    PROMPT="%{$fg[red]%}%~%{$reset_color%}:$> "
else
    PROMPT="%{$fg[cyan]%}%~%{$reset_color%}:$> "
fi

# Zsh customizations
export HISTFILE="$HOME/.cache/history.zsh"
export PATH="$HOME/bin:$PATH"
export GPGKEY="180C02EB09D0BE15"
export LESSHISTFILE="/dev/null"
umask 077

function total () {
    awk 'BEGIN { s = 0 } { s += $1 } END { print s }'
}

# Interact with vterm in emacs for notifications
_last_command=""

preexec() { 
  _last_command="$1" 
}

precmd() {
  [[ -n "$_last_command" ]] || return
  printf '\e]51;Epavel/vterm-command-finished "%s" %d\a' "$_last_command" "$?"
  _last_command=""
}
