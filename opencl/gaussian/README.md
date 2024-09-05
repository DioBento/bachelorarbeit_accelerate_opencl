# Gauss-Jordan Elimination <OpenCL>

This is taken from [Rodinia Benchmark Suite v3.1](https://github.com/yuhc/gpu-rodinia).

**compile with**
```sh
$ make
```

**USAGE**
```
$ ./opencl-gaussian [filename] [-hqt] [-p [int] -d [int]]
$ ./opencl-gaussian -s [size] $@

example:
    $ ./opencl-gaussian matrix4.txt
    $ ./opencl-gaussian -s 1024 $@

    -f [filename] The filename that holds the matrix data
    -s [int]      Generate matrix and rhs in specified size
    -h, --help    Display the help file
    -q            Quiet mode. Suppress all text output.
    -t            Print timing information.
    -v            Print init matrix and result.

    -p [int]     Choose the platform (must choose both platform and device)
    -d [int]     Choose the device (must choose both platform and device)


Notes: 1. The filename is required as the first parameter unless using size flag
       2. If you declare either the device or the platform,
          you must declare both.
```

**Adjustable work group size**

The kernel 2 has square shape <br>
The actually dimension = RD_WG_SIZE_1_0 * RD_WG_SIZE_1_1

USAGE:
```sh
$ make clean
$ make KERNEL_DIM="-DRD_WG_SIZE_0=16 -DRD_WG_SIZE_1_0=16 -DRD_WG_SIZE_1_1=16"
```
