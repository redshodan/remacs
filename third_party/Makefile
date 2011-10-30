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
PKGS=m2crypto
TINSTALL=$(M2CRYPTO_INSTALL)
PYVER=$(shell python -c "import sys; print sys.version[:3]")
BDIRS=$(TBUILD) $(M2CRYPTO_BUILD)/lib/python

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

clean:
	rm -rf $(TBUILD) $(TINSTALL) $(M2CRYPTO_SRC)
