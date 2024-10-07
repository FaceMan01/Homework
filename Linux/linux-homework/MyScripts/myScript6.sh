h=`hostname`
GTW="8.8.8.8"

ping -c 4 $h
traceroute $GTW

echo -n "This is done ...."
echo "----D.O.N.E.----"
