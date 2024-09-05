CC = gcc
#CFLAGS = -I/opt/cuda-11.8/targets/x86_64-linux/include
CFLAGS = 
WARNIN = -Wall -Wextra -Wpedantic -Wconversion -ggdb
LFLAGS = -lOpenCL

SRCS = main.c helper.c cbmp.c
OBJS = $(SRCS:.c=.o)

TARGET = opencl-blur

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(WARNIN) $^ -o $@ $(LFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) $(WARNIN) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET)
