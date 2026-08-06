# Thin wrapper around CMake for convenience.
# Prefer:  cmake -B build && cmake --build build && cmake --install build

BUILD_DIR ?= build
PREFIX    ?= /usr/local

.PHONY: all build clean install

all: build

build:
	cmake -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=RelWithDebInfo \
	  -DCMAKE_INSTALL_PREFIX=$(PREFIX)
	cmake --build $(BUILD_DIR) -j$$(nproc 2>/dev/null || echo 2)

clean:
	rm -rf $(BUILD_DIR)

install: build
	cmake --install $(BUILD_DIR)

# EOF #
