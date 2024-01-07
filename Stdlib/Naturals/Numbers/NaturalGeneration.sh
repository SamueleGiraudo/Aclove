max=127

echo "!../Zero" > "0.acl"
echo >> "0.acl"

for i in $(seq 1 $max); do
    prev=$(($i - 1))
    printf "!../Succ !%d\n" $prev > "$i.acl"
    echo >> "$i.acl"
done

