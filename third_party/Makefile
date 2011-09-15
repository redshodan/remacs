BUILD=../build
TBUILD=$(BUILD)/third_party
M2CRYPTO_VER=0.21.1
M2CRYPTO_TAR=M2Crypto-$(M2CRYPTO_VER).tar.gz
M2CRYPTO_SRC=M2Crypto-$(M2CRYPTO_VER)
M2CRYPTO_BUILD=$(TBUILD)/m2crypto
M2CRYPTO_INSTALL=$(BUILD)/python/M2Crypto
PKGS=m2crypto
PYVER=$(shell python -c "import sys; print sys.version[:3]")


all: $(PKGS)

$(BDIRS):
	mkdir -p $@

m2crypto: $(M2CRYPTO_INSTALL)

$(M2CRYPTO_INSTALL): $(M2CRYPTO_BUILD)
	rm -rf $@
	cp -r $(M2CRYPTO_BUILD)/lib/python$(PYVER)/site-packages/M2Crypto $@

$(M2CRYPTO_BUILD): $(BDIRS) $(M2CRYPTO_TAR)
	rm -rf $(M2CRYPTO_SRC)
	tar -zxf $(M2CRYPTO_TAR)
	(cd $(M2CRYPTO_SRC); python setup.py build)
	(cd $(M2CRYPTO_SRC); python setup.py install --prefix ../$(M2CRYPTO_BUILD))

clean:
	rm -rf $(TBUILD)
