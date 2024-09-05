# Nearest Neighbor <OpenCL>

This is taken from [Rodinia Benchmark Suite v3.1](https://github.com/yuhc/gpu-rodinia).

The Nearest Neighbor application computes the nearest location to a specific
latitude and longitude for a number of hurricanes (data from: http://weather.unisys.com/hurricane/).

**compile with**
```sh
$ make
```

**USAGE**
```sh
$ opencl-nn [filename] -r [int] -lat [float] -lng [float] [-hqt] [-p [int] -d [int]]

example:
$ ./nearestNeighbor filelist.txt -f . -lat 30 -lng 90

filename     the filename that lists the data input files
-r [int]     the number of records to return (default: 10)
-lat [float] the latitude for nearest neighbors (default: 0)
-lng [float] the longitude for nearest neighbors (default: 0)

-h, --help   Display the help file
-q           Quiet mode. Suppress all text output.
-t           Print timing information.

-p [int]     Choose the platform (must choose both platform and device)
-d [int]     Choose the device (must choose both platform and device)


Notes: 1. The filename is required as the first parameter.
       2. If you declare either the device or the platform,
          you must declare both.
```
