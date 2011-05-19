
print() {
    xsetroot -name "$@"
}

email() {
    local A
    A=`cat mail/inbox.spool | grep ^From\  | wc -l`

    if [[ -z "$A" || "$A" = "0" ]]; then
        return
    else
        echo "| [$A] "
        return
    fi
}

while sleep 3; do
    print "`email`| `dropbox status` | `date +%H:%M`"
done
