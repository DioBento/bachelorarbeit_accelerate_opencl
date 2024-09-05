# Gauss-Jordan Elimination <Accelerate>

Compile at project root alltogether or separately
```sh
$ cd ~/Bachelorarbeit-Accelerate-OpenCL/accelerate/

# to build all together
$ stack build

# to build specifically
$ stack build accelerate-gaussian

$ stack install

examples:
    $ accelerate-gaussian -s 1024 -v --llvm-ptx
    $ accelerate-gaussian -f matrix.txt --llvm-ptx
```
