BUILD=../build
TBUILD=$(BUILD)/third_party
M2CRYPTO_VER=0.21.1
M2CRYPTO_TAR=M2Crypto-$(M2CRYPTO_VER).tar.gz
M2CRYPTO_SRC=M2Crypto-$(M2CRYPTO_VER)
M2CRYPTO_BUILD=$(TBUILD)/m2crypto
M2CRYPTO_EGG=$(M2CRYPTO_BUILD)/lib/python2.6/site-packages/M2Crypto-$(M2CRYPTO_VER)-py$(PYVER).egg-info
M2CRYPTO_INSTALL=$(BUILD)/python/M2Crypto
PKGS=m2crypto
TINSTALL=$(M2CRYPTO_INSTALL)
PYVER=$(shell python -c "import sys; print sys.version[:3]")

all: $(PKGS)

m2crypto: $(M2CRYPTO_INSTALL)

$(M2CRYPTO_INSTALL): $(M2CRYPTO_EGG)
	rm -rf $@
	cp -r $(M2CRYPTO_BUILD)/lib/python$(PYVER)/site-packages/M2Crypto $@

$(M2CRYPTO_EGG): $(M2CRYPTO_SRC)
	(cd $(M2CRYPTO_SRC); python setup.py build)
	(cd $(M2CRYPTO_SRC); python setup.py install --prefix ../$(M2CRYPTO_BUILD))

$(M2CRYPTO_SRC): $(M2CRYPTO_TAR)
	rm -rf $(M2CRYPTO_SRC)
	tar -zxf $(M2CRYPTO_TAR)
	patch -p 1 < m2crypto.patch

clean:
	rm -rf $(TBUILD) $(TINSTALL)
