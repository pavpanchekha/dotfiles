
print() {
    xsetroot -name "$@"
}

while sleep 3; do
    print "| `date +%H:%M`"
done
