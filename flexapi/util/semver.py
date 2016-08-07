# Flexlay - A Generic 2D Game Editor
#
# ISC License
# Copyright (C) 2016 Karkus476 <karkus476@yahoo.com>
#
# Permission to use, copy, modify, and/or distribute this software for
# any purpose with or without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR ON SEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
# PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

import re

from ..flexlay_error import FlexlayError

# The regex pattern which additional prerelease and build metadata must follow
additionals_pattern = re.compile(r"^([a-zA-Z0-9-]+\.)*[a-zA-Z0-9-]+$")

class SemVer:
    """A simple SemVer versioning class

    See semver.org for more details.
    Members:
    major - Backwards incompatible changes
    minor - Backwards compatible features
    bugfix - Backwards compatible bugfixes
    Please use getters and setters for these:
    prerelease - Any release before a full-blown release, subject to changes
    build_metadata - Has no affect on comparison operators

    You can use <, >, <=, >=, ==, and != to compare SemVer versions.
    """
    def __init__(self, major=0, minor=0, bugfix=0):
        self.major = major
        self.minor = minor
        self.bugfix = bugfix
        self.prerelease = None
        self.build_metadata = None

    def set_prerelease(self, prerelease):
        """Set the prerelease to a string value.

        prerelease - Must be a dot-separated list of alphanumeric characters and hypens
                    e.g. "test-build.5"
        """
        if prerelease is not None and additionals_pattern.match(prerelease) is None:
            raise FlexlayError("Prerelease text does not match "\
                               "expected pattern!")
        else:
            self.prerelease = prerelease

    def get_prerelease(self):
        """Returns the current value of the prerelease, None if unspecified."""
        return self.prerelease

    def set_build_metadata(self, build_metadata):
        """Set metadata providing build detail.

        build_metadata - Must be a dot-separated list of alphanumeric characters and hypens
        """
        if build_metadata is not None and additionals_pattern.match(build_metadata) is None:
            raise FlexlayError("Build metadata text does not match "\
                               "expected pattern!")
        else:
            self.build_metadata = build_metadata

    def get_build_metadata(self):
        """Returns the current value of the build_metadata. None if unspecified,"""
        return self.build_metadata

    def __str__(self):
        return str(self.major) + "." + str(self.minor) + "." + str(self.bugfix)\
            + ("-" + self.prerelease if self.prerelease is not None else "")\
            + ("+" + self.build_metadata if\
               self.build_metadata is not None else "")

    def __eq__(self, other):
        if isinstance(other, SemVer):
            return self.major == other.major and \
                   self.minor == other.minor and \
                   self.bugfix == other.bugfix and \
                   (self.prerelease is None) == (other.prerelease is None)
        else:
            return super().__eq__(other)

    def __ne__(self, other):
        if isinstance(other, SemVer):
            return not self.__eq__(other)
        else:
            return super().__ne__(other)

    def __gt__(self, other):
        if isinstance(other, SemVer):
            if self.major != other.major:
                return self.major > other.major
            elif self.minor != other.minor:
                return self.minor > other.minor
            elif self.bugfix != other.bugfix:
                return self.bugfix > other.bugfix
            else:
                return self.prerelease is None and other.prerelease is not None

        else:
            return super().__gt__()

    def __ge__(self, other):
        if isinstance(other, SemVer):
            if self.major != other.major:
                return self.major > other.major
            elif self.minor != other.minor:
                return self.minor > other.minor
            elif self.bugfix != other.bugfix:
                return self.bugfix > other.bugfix
            else:
                return self.prerelease is None or other.prerelease is not None
        else:
            return super().__ge__()

    def __lt__(self, other):
        if isinstance(other, SemVer):
            if self.major != other.major:
                return self.major < other.major
            elif self.minor != other.minor:
                return self.minor < other.minor
            elif self.bugfix != other.bugfix:
                return self.bugfix < other.bugfix
            else:
                return self.prerelease is not None and other.prerelease is None
        else:
            return super().__lt__()

    def __le__(self, other):
        if isinstance(other, SemVer):
            if self.major != other.major:
                return self.major < other.major
            elif self.minor != other.minor:
                return self.minor < other.minor
            elif self.bugfix != other.bugfix:
                return self.bugfix < other.bugfix
            else:
                return self.prerelease is not None or other.prerelease is None
        else:
            return super().__le__()

