BUILD=../build
PYSYS=$(shell python -c 'import platform, sys; print "%s-%s-%d.%d" % (platform.system().lower(), platform.machine(), sys.version_info[0], sys.version_info[1])')

all: idle

idle: $(BUILD)/python/remacs/idle.so

$(BUILD)/python/remacs/idle.so: $(BUILD)/lib/lib.$(PYSYS)/idle.so
	cp -f $^ $@

$(BUILD)/lib/lib.$(PYSYS)/idle.so: setup.py idle.c
	python setup.py build --build-base $(BUILD)/lib

.PHONY: idle
