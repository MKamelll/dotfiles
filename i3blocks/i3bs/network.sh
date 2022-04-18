CON_NAME=$(nmcli device | awk '{ if ($3 == "connected") print "up:" $4}')
STRENGTH=$(nmcli device wifi | awk '{ if ($1 == "*") print $8}')

echo "$CON_NAME $STRENGTH%"
