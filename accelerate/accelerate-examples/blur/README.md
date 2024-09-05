# Gaussian Blur <Accelerate>

This is a slightly cut down official example of the Gaussian blur taken from [github](https://github.com/tmcdonell/colour-accelerate/blob/master/examples/Blur.hs).

**USAGE** <br>
Needs to be compiled from project root alltogether or separately
```sh
$ cd /path/to/Bachelorarbeit-Accelerate-OpenCL/accelerate/

# to build all together
$ stack build

# to build this specifically
$ stack build accelerate-blur

$ stack install

example:
    $ accelerate-blur [file.bmp] --llvm-ptx
```
