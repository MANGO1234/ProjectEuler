$d="../problems/python/"+$args[0]+"/"
mkdir $d
copy env.yml $d
copy main.py $d
g c -m $args[0]
g p