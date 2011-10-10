set BROWSER w3m
set CDPATH .
set -x PATH ~/usr/bin /bin /usr/bin /usr/local/bin /sbin /usr/sbin
set -x EDITOR vim
set -x PAGER less
set -x TEXINPUTS ".:$HOME/dev/resume.tex:$HOME/dev/simple.tex:"
set -x GPGKEY 371E352C

# No reason to save my less history
set -x LESSHIST -

# Mathematica fonts need setting over SSH
if [ -d /usr/local/mathematica/fonts/Type1 ];
    xset fp+ /usr/local/mathematica/fonts/Type1
    xset fp+ /usr/local/mathematica/fonts/BDF
end

function fish_greeting -d "Silence the greeting"
end

function fish_title -d "Emacs doesn't like title changing"
end

function fish_prompt -d "Display the prompt"
    printf '%s%s%s:$> ' (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end

function project -d "Display screen on projector"
    set OUTPUT (xrandr | grep "connected" | cut -d\  -f1 | grep -v LVDS1 | head -n1)
    set MODE (xrandr | grep $OUTPUT -a1 | tail -n1 | cut -f4 -d\ )
    xrandr --output $OUTPUT --mode $MODE
end

umask 077

alias math "rlwrap math"
alias sbcl "rlwrap sbcl"
alias scheme "rlwrap scheme"
alias plan9 "qemu-kvm -enable-kvm /media/virtual/plan9/plan9.qcow -no-acpi -net nic,vlan=1 -net user,vlan=1"
alias zephyr "tmux -L zephyr attach"
alias edconfigure "vim ~/.config/fish/config.fish"
alias reconfigure ". ~/.config/fish/config.fish"
alias py "ipython"
