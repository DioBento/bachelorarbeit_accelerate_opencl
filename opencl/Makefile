PREFIX ?= /usr/local
BIN_DIR = $(PREFIX)/bin

all:
	make -C gaussian
	make -C nn
	make -C blur

clean:
	make -C gaussian clean
	make -C nn clean
	make -C blur clean

# not feasable because rodinia tries to write it's
# benchmark in a file and put it where the executable
# is but failing because it doesn't have permission so
# no performance is being printed to console
#
# better execute from executable root
install: all
	mkdir -p $(BIN_DIR)

	sudo cp gaussian/opencl-gaussian $(BIN_DIR)
	sudo cp nn/opencl-nn $(BIN_DIR)
	sudo cp blur/opencl-blur $(BIN_DIR)

uninstall: clean
	sudo rm -f $(BIN_DIR)/opencl-gaussian
	sudo rm -f $(BIN_DIR)/opencl-nn
	sudo rm -f $(BIN_DIR)/opencl-blur
