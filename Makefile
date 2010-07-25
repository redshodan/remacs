all: debug

debug:
	cd android; ant debug

release:
	cd android; ant release

clean:
	cd android; ant clean

install: install-device

install-emulator:
	adb -e uninstall org.codepunks.remacs; adb -e install android/bin/remacs-debug.apk

install-device:
	adb -d uninstall org.codepunks.remacs; adb -d install android/bin/remacs-debug.apk
