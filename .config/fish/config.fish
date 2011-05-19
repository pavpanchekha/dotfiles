set BROWSER w3m
set CDPATH .
set PATH ~/usr/bin /bin /usr/bin /usr/local/bin
set EDITOR vim
set PAGER less
set TEXINPUTS ".:$HOME/dev/resume.tex:$HOME/dev/simple.tex:"

function fish_greeting -d "Silence the greeting"
end

function fish_prompt -d "Display the prompt"
    printf '%s%s%s:$> ' (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end

umask 077
