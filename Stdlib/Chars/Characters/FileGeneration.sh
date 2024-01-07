echo "!../Char !../../Naturals/Zero" > "C_000.acl"
echo >> "C_000.acl"

for i in $(seq -w 1 127); do
    prev=$(printf "%03d" $((10#$i - 1)))
    echo "!../Next !C_$prev" > "C_$i.acl"
    echo >> "C_$i.acl"
done

