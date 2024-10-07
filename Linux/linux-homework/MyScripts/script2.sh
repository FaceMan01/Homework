if	[ "$1" == "$2" ]; then
	echo "$1 eq $2"
else	echo "$1 not eq $2"
fi

if      [ "$1" > "$2" ]; then
        echo "ASCII: $1 > $2"
elif	[ "$1" == "$2" ]; then
	echo "ASCII: $1 = $2"
else    echo "ASCII: $1 < $2"
fi

if      [ -z "$1" ]; then
        echo "1 is empty"
else    echo "1 is not empty"
fi

if      [ -n "$2" ]; then
        echo "2 is not empty"
else    echo "2 is empty"
fi
