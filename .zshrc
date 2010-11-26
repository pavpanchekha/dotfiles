#!/usr/bin/zsh

setopt AUTO_PUSHD PUSHD_IGNORE_DUPS PUSHD_MINUS PUSHD_SILENT PUSHD_TO_HOME
setopt AUTO_LIST AUTO_MENU AUTO_PARAM_SLASH COMPLETE_IN_WORD GLOB_COMPLETE
unsetopt CLOBBER
setopt CORRECT_ALL MAIL_WARNING PATH_DIRS AUTO_CONTINUE

PATH="$PATH:$HOME/usr/bin"
TEXINPUTS=".:$HOME/dev/resume.tex:$HOME/dev/simple.tex"

export PATH TEXINPUTS

# Prompt
autoload colors; colors
export PROMPT="%{$fg[cyan]%}%2c%{$reset_color%}:%#> "

# Program definitions
EDITOR="vim"
PAGER=less
TERM=xterm

# Some Variables
HOSTNAME=`hostname`
TZ="America/New_York"
GPGKEY="0A7D7BAA"
PYTHONSTARTUP="$HOME/.pythonrc"
MAIL="/var/spool/mail/pavpanchekha"
umask 077

alias py=python

export EDITOR PAGER TERM HOSTNAME GPGKEY PYTHONSTARTUP MAIL

# Short programs
alias temp="cat > /dev/null"
calc() { awk "BEGIN{ print $* }" ;}
pset() { run -c $1 -o /tmp/`basename $PWD`.pdf && scp /tmp/`basename $PWD`.pdf mit:print-queue ;}
alarm() { echo notify-send "'$2'" -u critical -i gtk-info | at `date +%H:%M --date="+$1 min"` 2>&1 >/dev/null;}

# Mathematica fonts need setting over SSH
if [ -d /usr/local/mathematica/fonts/Type1 ]; then
    xset fp+ /usr/local/mathematica/fonts/Type1
    xset fp+ /usr/local/mathematica/fonts/BDF
fi

