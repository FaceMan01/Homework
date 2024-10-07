for myfile in `ls *.txt`; do
	cat $myfile
done

echo -n "" > myfile.txt
for x in {1..4}; do
	echo "X = $x" >> myfile.txt
done

for (( i=1; i<=7; i++ )); do
	echo -n "$i " >> myfile.txt
done
echo "" >> myfile.txt
