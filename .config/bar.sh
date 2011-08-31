#!/bin/sh

PRINT=echo

email() {
    local A
    if [[ ! -d "$HOME/mail/inbox/new" ]]; then
        return
    fi

    A=`ls ~/mail/inbox/new  | wc -l`

    if [[ -z "$A" || "$A" = "0" ]]; then
        return
    else
        echo "| Mail: $A "
        return
    fi
}

batt () {
    acpi -b | cut -f4 -d" " | sed 's/,//g'
}

main () {
    $PRINT "`email`| Batt: `batt` | `dropbox status` | `date +%H:%M`"
}

if [[ "$1" = "dwm" ]]; then
    PRINT="xsetroot -name"
    while sleep 5; do
        main
    done
elif [[ "$1" = "tmux" ]]; then
    PRINT="echo" # tmux just takes command output
    main
else
    main
fi
