##
## Top level targets: 
##    build - Builds everything
##    python - Builds python
##    debug - Builds debug android client
##    release - Builds release android client
##    install - Installs android client to physical device
##    install-device - Installs android client to physical device
##    install-emualtor - Installs android client to emulator
##    tests - Builds and runs all tests
##

BUILD=build/python
SRC=$(shell find python -name '*.py')
OBJS=$(patsubst %, build/%c, $(SRC))
TSRC=$(shell find tests -name '*.py')
TOBJS=$(patsubst %, build/%c, $(TSRC))
DIRS=$(shell find python tests -type d)
BDIRS=$(patsubst %, build/%, $(DIRS)) build/runs
PYVER=$(shell python -c "import sys; print sys.version[:3]")

all: python

build: $(BDIRS) python debug

$(BDIRS):
	mkdir -p $@

python: $(BDIRS) $(OBJS) idle

idle: $(BUILD)/remacs/idle.so
$(BUILD)/remacs/idle.so: lib/idle.c
	gcc -shared -fPIC -Wall -I /usr/include/python$(PYVER) lib/idle.c -lpython$(PYVER) -lX11 -lXss -lXtst -o $(BUILD)/remacs/idle.so

tests: $(BDIRS) clean-run python $(TOBJS)
	(cd build/runs; python ../tests/run.pyc)

clean-run:
	rm -rf build/runs/*

clean: clean-android
	rm -rf build
	find -name '*.pyc' | xargs rm -f


## Android client rules
debug:
	cd android; ant debug

release:
	cd android; ant release

clean-android:
	cd android; ant clean

install: install-device

install-emulator:
	adb -e uninstall org.codepunks.remacs; adb -e install android/bin/remacs-debug.apk

install-device:
	adb -d uninstall org.codepunks.remacs; adb -d install android/bin/remacs-debug.apk


### Automatics
$(BUILD)/%.pyc: python/%.py
	python -c "import sys, py_compile; py_compile.compile(sys.argv[1], sys.argv[2], doraise=True)" $< $@

build/tests/%.pyc: tests/%.py
	python -c "import sys, py_compile; py_compile.compile(sys.argv[1], sys.argv[2], doraise=True)" $< $@

.PHONY: build python idle tests clean-run
