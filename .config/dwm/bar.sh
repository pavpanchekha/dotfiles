
print() {
    xsetroot -name "$@"
}

email() {
    local A
    A=`cat mail/*.spool | grep ^From\  -c`

    if [[ -z "$A" || "$A" = "0" ]]; then
        return
    else
        echo "| [$A] "
        return
    fi
}

batt () {
    acpi -b | cut -f4 -d" " | sed 's/,//g'
}

while sleep 3; do
    print "`email`| `batt` | `dropbox status` | `date +%H:%M`"
done
