tools:
	gprbuild $(GPROPTS) -P tools

install:
	gprbuild $(GPROPTS) -P tools -XBUILD_MODE=release
	gprinstall --mode=usage --install-name=tools -f $(GPROPTS) -P tools -XBUILD_MODE=release
