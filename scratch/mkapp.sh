#!/bin/sh

usage() {
    echo "usage: mkapp.sh <executable-name> [icon]\n  icon defaults to <executable-name>.png."
    exit 2
}

 APP=$1
[ -n "$APP" ] || usage
mkdir -p "$APP.app/Contents/MacOS"
cp "$1" "$APP.app/Contents/MacOS"
ICONDIR="$APP.app/Contents/Resources/$APP.iconset"
ORIGICON=${2:-"$APP.png"}

mkdir -p "$ICONDIR"

# Normal screen icons
for SIZE in 16 32 64 128 256 512; do
sips -z "$SIZE" "$SIZE" "$ORIGICON" --out "$ICONDIR/icon_${SIZE}x${SIZE}.png" > /dev/null
done

# Retina display icons
for SIZE in 32 64 256 512; do
sips -z "$SIZE" "$SIZE" "$ORIGICON" --out "$ICONDIR"/icon_$(expr "$SIZE" / 2)x$(expr "$SIZE" / 2)x2.png > /dev/null
done

# Make a multi-resolution Icon
iconutil -c icns -o "$APP".app/Contents/Resources/AppIcon.icns "$ICONDIR" > /dev/null
rm -rf "$ICONDIR" #it is useless now

echo "
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
    <key>CFBundleIconFile</key>
    <string>AppIcon</string>
    <key>CFBundleIconName</key>
    <string>AppIcon</string>
</dict>
</plist>
" > "$APP.app/Contents/Info.plist"

echo "Built $APP.app."