BUILD=../build
TBUILD=$(BUILD)/third_party

M2CRYPTO_VER=0.21.1
M2CRYPTO_TAR=M2Crypto-$(M2CRYPTO_VER).tar.gz
M2CRYPTO_SRC=M2Crypto-$(M2CRYPTO_VER)
M2CRYPTO_BUILD=$(TBUILD)/m2crypto
# M2CRYPTO_EGG=$(M2CRYPTO_BUILD)/lib/python/M2Crypto-$(M2CRYPTO_VER)-py$(PYVER)-*.egg
M2CRYPTO_EGG=$(M2CRYPTO_BUILD)/lib/python/site.py
M2CRYPTO_INSTALL=$(BUILD)/python/M2Crypto
M2CRYPTO_ABS_BUILD=$(shell python -c "import os.path; print os.path.abspath(\"$(M2CRYPTO_BUILD)\");")

UNITTEST2_VER=0.5.1
UNITTEST2_TAR=unittest2-$(UNITTEST2_VER).tar.gz
UNITTEST2_SRC=unittest2-$(UNITTEST2_VER)
UNITTEST2_BUILD=$(TBUILD)/unittest2
# UNITTEST2_EGG=$(UNITTEST2_BUILD)/lib/python/Unittest2-$(UNITTEST2_VER)-py$(PYVER)-*.egg
UNITTEST2_EGG=$(UNITTEST2_BUILD)/lib/python/site.py
UNITTEST2_INSTALL=$(BUILD)/python/unittest2
UNITTEST2_ABS_BUILD=$(shell python -c "import os.path; print os.path.abspath(\"$(UNITTEST2_BUILD)\");")

PKGS=m2crypto unittest2
TINSTALL=$(M2CRYPTO_INSTALL) $(UNITTEST2_INSTALL)
PYVER=$(shell python -c "import sys; print sys.version[:3]")
BDIRS=$(TBUILD) $(M2CRYPTO_BUILD)/lib/python $(UNITTEST2_BUILD)/lib/python

all: $(BDIRS) $(PKGS)

$(BDIRS):
	mkdir -p $@

m2crypto: $(M2CRYPTO_INSTALL)

$(M2CRYPTO_INSTALL): $(M2CRYPTO_EGG)
	rm -rf $@
	cp -r $(M2CRYPTO_BUILD)/lib/python/M2Crypto-*egg/M2Crypto $@

$(M2CRYPTO_EGG): $(M2CRYPTO_SRC)
	(cd $(M2CRYPTO_SRC); python setup.py build)
	(cd $(M2CRYPTO_SRC); PYTHONPATH=$(M2CRYPTO_ABS_BUILD)/lib/python python setup.py install --home $(M2CRYPTO_ABS_BUILD))

$(M2CRYPTO_SRC): $(M2CRYPTO_TAR)
	rm -rf $(M2CRYPTO_SRC)
	tar -zxf $(M2CRYPTO_TAR)
	(cd $(M2CRYPTO_SRC); patch -p0 < ../m2crypto.patch)
	(cd $(M2CRYPTO_SRC); patch -p0 < ../m2crypto-nbio.patch)
	(cd $(M2CRYPTO_SRC); patch -p0 < ../m2crypto-x509.patch)

unittest2: $(UNITTEST2_INSTALL)

$(UNITTEST2_INSTALL): $(UNITTEST2_EGG)
	rm -rf $@
	cp -r $(UNITTEST2_BUILD)/lib/python/unittest2-*egg/unittest2 $@

$(UNITTEST2_EGG): $(UNITTEST2_SRC)
	(cd $(UNITTEST2_SRC); python setup.py build)
	(cd $(UNITTEST2_SRC); PYTHONPATH=$(UNITTEST2_ABS_BUILD)/lib/python python setup.py install --home $(UNITTEST2_ABS_BUILD))

$(UNITTEST2_SRC): $(UNITTEST2_TAR)
	rm -rf $(UNITTEST2_SRC)
	tar -zxf $(UNITTEST2_TAR)

clean:
	rm -rf $(TBUILD) $(TINSTALL) $(M2CRYPTO_SRC) $(UNITTEST2_SRC)
