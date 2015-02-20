

function fish_prompt -d "The interactive shell prompt"
    if [ $status -eq 0 ]
        set arrow (printf "%s>" (set_color green))
    else
        set arrow (printf "%s>" (set_color red))
    end

    set branch (git rev-parse --abbrev-ref HEAD 2> /dev/null)

    if [ $status -gt 0 -o "$branch" = "master" ]
        set branch ""
    else
        set branch (printf "@%s" $branch)
    end

    set path (printf "%s%s%s" (set_color blue) (prompt_pwd) (set_color normal))

    printf "%s%s%s%s " $path $branch $arrow (set_color normal)
end