#!/usr/bin/env zsh
set -euo pipefail

ROOT="$HOME/bin"
LABEL="com.pavpan.top-cpu-widget"
PLIST_SRC="$ROOT/$LABEL.plist"
PLIST_DST="$HOME/Library/LaunchAgents/$LABEL.plist"

if [[ ! -f "$PLIST_SRC" ]]; then
  echo "Missing plist source: $PLIST_SRC" >&2
  exit 1
fi

cd "$ROOT"
swiftc -O "$ROOT/top-cpu-widget.swift" -o "$ROOT/top-cpu-widget"

mkdir -p "$HOME/Library/LaunchAgents"
cp "$PLIST_SRC" "$PLIST_DST"

launchctl bootout "gui/$(id -u)/$LABEL" >/dev/null 2>&1 || true
launchctl bootstrap "gui/$(id -u)" "$PLIST_DST"
launchctl enable "gui/$(id -u)/$LABEL"
launchctl kickstart -k "gui/$(id -u)/$LABEL"

echo "Installed and started: $LABEL"
echo "To inspect: launchctl print gui/$(id -u)/$LABEL"
