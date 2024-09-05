# Accelerate Examples

**build and install all with** <br>
You need to be in project root.
```sh
$ cd ~/path/to/Bachelorarbeit-Accelerate-OpenCL/accelerate/
$ stack build
$ stack install
```

**Example usage**
```sh
$ accelerate-gaussian -s [int] -v --llvm-ptx
$ accelerate-nn --llvm-ptx
$ accelerate-blur [file.bmp] --llvm-ptx

NOTE: If you don't set the --llvm-ptx flag the
      program will run on the CPU instead
```
