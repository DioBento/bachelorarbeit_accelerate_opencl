# Bachelorarbeit Accelerate-OpenCL

## Accelerate

The blur example is taken from an official example on [github](https://github.com/tmcdonell/colour-accelerate/blob/master/examples/Blur.hs).

### Dependencies

- [LLVM](http://llvm.org) v15
- [libFFI](http://sourceware.org/libffi/) (for native backend for CPUs)
- [CUDA](https://developer.nvidia.com/cuda-downloads) (for PTX backend for NVidia GPUs)

### Build and run Accelerate

**Navigate to project root then compile and install with stack**

```sh
$ cd accelerate
$ stack build
$ stack install
```

**Run executables**
```sh
examples:
$ accelerate-gaussian -s 1024 -v --llvm-ptx
$ accelerate-blur <picture.bmp> --llvm-ptx
$ accelerate-nn --llvm-ptx
```

---

## OpenCL

Some OpenCL examples are taken from [Rodinia Benchmark Suite v3.1](https://github.com/yuhc/gpu-rodinia).

### Build and run OpenCL

**Navigate to project root then compile with makefile**

```sh
$ cd opencl
$ make
```

**Run executables**

Reccomended to run in the executables directory.
```sh
examples:
# cd /path/to/Bachelorarbeit-Accelerate-OpenCL/opencl/gaussian
$ ./opencl-gaussian -s 1024 -v

# cd /path/to/Bachelorarbeit-Accelerate-OpenCL/opencl/blur
$ ./opencl-blur <picture.bmp>

# cd /path/to/Bachelorarbeit-Accelerate-OpenCL/opencl/nn
$ ./opencl-nn
```
