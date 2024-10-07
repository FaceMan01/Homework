#!/bin/bash

if  [ "$1" -lt 5 ];
then echo "$1 < 5"
elif [ "$1" -ge 10 ];
then echo "$1 >= 10"
else echo "5 <= $1 < 10"
fi
