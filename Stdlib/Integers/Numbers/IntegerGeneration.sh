max=127

echo "!../Zero" > "0.acl"
echo >> "0.acl"

for i in $(seq 1 $max); do
    prev=$(($i - 1))
    printf "!../Incr !%d\n" $prev > "$i.acl"
    echo >> "$i.acl"
done

echo "!../Decr !0" > "-1.acl"
echo >> "-1.acl"

for i in $(seq 2 $max); do
    prev=$(($i - 1))
    printf "!../Decr !-%d\n" $prev > "-$i.acl"
    echo >> "-$i.acl"
done

