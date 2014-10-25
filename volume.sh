vol=$(amixer get Master |
    awk -F'[]%[]' '/%/ {if ($5 == "off") { print "<fc=#aa0000>V "$2 "</fc>" }
                       else { print "V "$2 }}' |
    head -n 1)
echo $vol

exit 0
