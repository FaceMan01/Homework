sum=0
pr=0

myFun()
{
	sum=$(($1+$2))
	pr=$(($1*$2))
}

echo "Hello, $1. It is $0"

myFun $2 $3
echo "sum = $sum"
echo "pr = $pr"
