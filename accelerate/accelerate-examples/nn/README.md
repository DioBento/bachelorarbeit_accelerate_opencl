# Nearest Neighbour <Accelerate>

Compile at project root alltogether or separately
```sh
$ cd /path/to/Bachelorarbeit-Accelerate-OpenCL/accelerate/

# to build all together
$ stack build

# to build specifically
$ stack build accelerate-nn

$ stack install

examples:
    $ accelerate-nn --lat 30 --lon 90 --llvm-ptx
```

The executable looks for a database.txt file in the current directory to read the input from.
