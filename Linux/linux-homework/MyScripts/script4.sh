echo -n "START = "
read START

echo -n "END = "
read END

#echo "$START $END"

if [ $START -lt $END ]; then
#	echo "START > END"
	while [ $START -le $END ]; do
		echo -n "$START "
		START=$(($START+1))
#		let START=START+1
#		let START+=1
	done
elif [ $START -ge $END ]; then
#	echo "START < END"
	while [ $START -ge $END ]; do
		echo -n "$START "
		START=$(($START-1))
	done
else echo "START = END, or ERROR"
fi

echo ""
