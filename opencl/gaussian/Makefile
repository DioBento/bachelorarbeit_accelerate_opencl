include ../common/make.config

OPENCL_INC = $(NV_OPENCL_INC)
OPENCL_LIB = $(NV_OPENCL_LIB)

CC = g++
SRC = clutils.cpp gaussianElim.cpp ../util/utils.cpp
CC_FLAGS = -g -O3
EXE = opencl-gaussian

release:
	$(CC) $(KERNEL_DIM) $(CC_FLAGS) -fopenmp $(SRC) -o $(EXE) -I$(OPENCL_INC) -L$(OPENCL_LIB) -lOpenCL -I../util -DTIMING -Wno-unused-result

run:
	./gaussian.out -s 2048 -p 0 -d 0

clean: 
	rm -f $(EXE) 
