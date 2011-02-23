
print() {
    xsetroot -name "$@"
}

while sleep 3; do
    print "| `dropbox status` | `date +%H:%M`"
done
