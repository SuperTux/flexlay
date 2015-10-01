SOURCES := $(wildcard \
  flexlay/*.py \
  flexlay/gui/*.py \
  flexlay/tools/*.py \
  flexlay/math/*.py \
  supertux/*.py \
  tests/*.py)

all: autopep flake test

autopep:
	autopep8  --max-line=120  --in-place $(SOURCES)

test:
	python3 -m unittest discover -s tests/

flake:
	python3 -m flake8.run --max-line-length=120 $(SOURCES)

PYLINT_TARGETS := $(addprefix .pylint/, $(SOURCES))

$(PYLINT_TARGETS): .pylint/%.py: %.py
	mkdir -p $(dir $@)
	epylint $<
	touch $@

pylint: $(PYLINT_TARGETS)
	echo $<

clean:
	rm -vrf .pylint/

.PHONY: autopep test flake pylint clean

# EOF #
