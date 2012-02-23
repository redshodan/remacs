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

all: third_party python

build: $(BDIRS) python debug

$(BDIRS):
	mkdir -p $@

python: $(BDIRS) $(OBJS) idle

idle:
	make -C lib

third_party: $(BDIRS)
	make -C third_party

tests: $(BDIRS) clean-run python $(TOBJS)
	(cd build/runs; python ../tests/run.pyc $(ARGS))

clean-run:
	rm -rf build/runs/*

third_party-clean:
	make -C third_party clean

clean: clean-android third_party-clean
	rm -rf build
	find -name '*.pyc' | xargs rm -f


## Android client rules
debug:
	make -C android debug

release:
	make -C android release

clean-android:
	@(make -C android clean)

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

.PHONY: build python idle tests clean-run third_party
