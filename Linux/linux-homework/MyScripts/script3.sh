echo "_______START______"

read -p "Enter something: " x
case $x in
	[1-5]) echo "1-5";;
	[6-9]) echo "6-9";;
	"str") echo "string";;
	*) echo "UNKNOWN";;
esac
