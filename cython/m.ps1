$d="../problems/python/"+$args[0]+"/"
mkdir $d
copy env.yml $d
copy euler_cython.pyx $d
copy main.py $d
g commit -m $args[0]
g p