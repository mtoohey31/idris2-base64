BUILD_TTC = build/ttc/Base64.ttc
PACKAGE_PATH = build/package-path
PACKAGE_PATH_TTC = $(PACKAGE_PATH)/base64/Base64.ttc
TEST_EXECUTABLE = test/build/exec/base64-test

$(BUILD_TTC): base64.ipkg src/Base64.idr
	idris2 --build $<

$(PACKAGE_PATH_TTC): $(BUILD_TTC)
	mkdir -p $$(dirname $@)
	cp $< $@

.PHONY: test
test: $(TEST_EXECUTABLE)
	$<

$(TEST_EXECUTABLE): test/test.ipkg test/src/Main.idr $(PACKAGE_PATH_TTC)
	cd test && \
	    IDRIS2_PACKAGE_PATH=../$(PACKAGE_PATH) idris2 --build test.ipkg

.PHONY: clean
clean:
	rm -rf build/ test/build/ result
