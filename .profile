#!/bin/sh

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

# Bash customizations
HISTIGNORE="&:bg:fg:exit: *" # Ignore duplicate commands, bg, fg, exit
HISTFILE=".cache/history.bash"
export PS1 CDPATH HISTIGNORE HISTFILE

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


function to-emacs () {
    bg
    emacsclient -c -e '(term-simple-send (get-buffer-process (ansi-term "/bin/bash")) "reptyr '`jobs -p %+`'")'
    kill $$
}

alias edconfigure="$EDITOR ~/.profile"
alias reconfigure=". ~/.profile"
alias ed="emacsclient -t-a"

# Switch to using non-destructive trashing when possible.
alias rm="rm -i"

function pclone () {
    local cmd
    cmd="ssh -i ~/.ssh/p92_rsa -F /dev/null"
    GIT_SSH_COMMANd="$cmd" git clone "$@"
}

function pconfig () {
    cmd="ssh -i ~/.ssh/p92_rsa -F /dev/null"
    git config core.sshCommand "$cmd"
}

function total () {
    awk 'BEGIN { s = 0 } { s += $1 } END { print s }'
}
