# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2018 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

SOURCES := $(wildcard flexlay/*.py flexlay/*/*.py supertux/*.py tests/*.py)

default: flake mypy # test

all: autopep flake test pylint

autopep:
	autopep8 --max-line=120 --in-place --aggressive $(SOURCES)

test:
	python3 -m unittest discover -v -s tests/

mypy:
	mypy \
        --incremental \
	--ignore-missing-imports \
	--follow-imports skip \
	--warn-return-any \
	--warn-unused-ignores \
	--warn-incomplete-stub \
	--warn-redundant-casts \
	$(SOURCES)

mypyverbose:
	export MYPYPATH=/usr/lib/python3/dist-packages/; \
	mypy \
        --incremental \
	--ignore-missing-imports \
	--follow-imports silent \
	--check-untyped-defs \
	--warn-return-any \
	--warn-unused-ignores \
	--warn-incomplete-stub \
	--warn-redundant-casts \
	$(SOURCES) | \
        grep -v ^/usr | \
	fgrep -v '"pyqtSignal" has no attribute' | \
	grep -v 'Callable.*has no attribute "connect' | \
	fgrep -v 'No overload variant of "QAction" matches argument types'

#	--disallow-any unimported,unannotated,decorated,explicit,generics \
#	--disallow-any unimported,expr,unannotated,decorated,explicit,generics \

flake:
	flake8 --max-line-length=120 --ignore=N802,W504 $(SOURCES)

PYLINT_TARGETS := $(addprefix .pylint/, $(SOURCES))

$(PYLINT_TARGETS): .pylint/%.py: %.py
	mkdir -p $(dir $@)
	PYTHONPATH=. epylint3 $< --rcfile=.pylintrc --max-line-length=120 --extension-pkg-whitelist=PyQt5
	touch $@

pylint: $(PYLINT_TARGETS)

clean:
	rm -vrf .pylint/

install:
	sudo -H pip3 install --force-reinstall --upgrade .

.PHONY: autopep test flake pylint clean all default run

.NOTPARALLEL: all

# EOF #
