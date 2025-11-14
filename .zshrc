#!/bin/zsh

# Load colors for prompt
autoload -U colors && colors

if [ "x$EUID" = "x0" ]; then
    export TMOUT=60
    PROMPT="%{$fg[red]%}%~%{$reset_color%}:$> "
else
    PROMPT="%{$fg[cyan]%}%~%{$reset_color%}:$> "
fi

if [ "x$TERM" = "xxterm" ]; then
    _set_terminal_title() {
        printf "\033]0;Zsh: %s@%s %s\007" "${USER}" "${HOSTNAME}" "${PWD/#$HOME/~}"
    }
    precmd_functions+=(_set_terminal_title)
fi

# Zsh customizations
HISTFILE=".cache/history.zsh"
export CDPATH HISTFILE

export XDG_CONFIG_HOME="$HOME/.config"
export VIMPERATOR_RUNTIME="$XDG_CONFIG_HOME/vimperator"
export VIMPERATOR_INIT=":source $VIMPERATOR_RUNTIME/vimperatorrc"
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'
export VIMDOTDIR="$XDG_CONFIG_HOME/vim"

if [ $0 = "bash" ]; then
    shopt -s cdspell # Spell-correct `cd _`
    shopt -s cmdhist # Add multiline commands as one-liners in my history
    source /usr/share/bash-completion/bash_completion &
    eval `dircolors -b`
fi

# Some actual environment variables
PATH="$HOME/bin:$PATH"
GPGKEY="180C02EB09D0BE15"
LESSHISTFILE="/dev/null"
LANG=en_US.UTF-8
export PATH TEXINPUTS GPGKEY LESSHISTFILE LANG
umask 077

alias ed="emacsclient -t-a"

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
