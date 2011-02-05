#!/usr/bin/zsh

setopt AUTO_PUSHD PUSHD_IGNORE_DUPS PUSHD_MINUS PUSHD_SILENT PUSHD_TO_HOME
setopt AUTO_LIST AUTO_MENU AUTO_PARAM_SLASH COMPLETE_IN_WORD GLOB_COMPLETE
unsetopt CLOBBER
setopt CORRECT_ALL MAIL_WARNING PATH_DIRS AUTO_CONTINUE

bindkey -e

# Prompt
autoload colors; colors
export PROMPT="%{$fg[cyan]%}%2c%{$reset_color%}:%#> "

PATH="$PATH:$HOME/usr/bin"
TEXINPUTS=".:$HOME/dev/resume.tex:$HOME/dev/simple.tex:"

export PATH TEXINPUTS
# Program definitions
EDITOR="vim"
PAGER=less
TERM=xterm

# Some Variables
TZ="America/New_York"
GPGKEY="371E352C"
PYTHONSTARTUP="$HOME/.pythonrc"
MAIL="$HOME/mail/inbox"
umask 077

alias py=python

export EDITOR PAGER TERM HOSTNAME GPGKEY PYTHONSTARTUP MAIL LANG LC_ALL

# Short programs
alias temp="cat > /dev/null"
calc() { awk "BEGIN{ print $* }" ;}
pset() { run -c $1 -o /tmp/`basename $PWD`.pdf && scp /tmp/`basename $PWD`.pdf mit:print-queue ;}
wiki() { dig +short txt $1.wp.dg.cx }

# Mathematica fonts need setting over SSH
if [ -d /usr/local/mathematica/fonts/Type1 ]; then
    xset fp+ /usr/local/mathematica/fonts/Type1
    xset fp+ /usr/local/mathematica/fonts/BDF
fi

FIGNORE=".o:~:.hi:.pyc"
HISTFILE="/tmp/zsh-history-pavpanchekha"

ssh-connect () {
    ssh-add
}

alias math="rlwrap math"
alias sbcl="rlwrap sbcl"
alias mpc="ssh media@server mpc"
alias plan9="qemu-kvm -enable-kvm /media/virtual/plan9/plan9.qcow -no-acpi -net nic,vlan=1 -net user,vlan=1"
