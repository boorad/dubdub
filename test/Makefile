include ../support/include.mk

SOURCE_FILES = $(wildcard *.erl)
SOURCE_MODULES = $(basename -s .beam $(wildcard *.beam))
TEST_ERL = erl -noshell -run

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

.PHONY: test
test:
	for i in $(SOURCE_FILES); do $(ERLC) $$i; done
	for i in $(SOURCE_MODULES); do echo "$(TEST_ERL) $$i test"; $(TEST_ERL) $$i test; done
