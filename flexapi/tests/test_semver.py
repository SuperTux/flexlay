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

import unittest

from flexapi.util.semver import SemVer
from flexapi.flexlay_error import FlexlayError

class SemVerTestCase(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_prerelease(self):
        semver = SemVer()
        # Check defaults to None
        self.assertIs(semver.get_prerelease(), None)
        # None alphanumeric
        self.assertRaises(FlexlayError, semver.set_prerelease, "a.@")
        self.assertRaises(FlexlayError, semver.set_prerelease, "a.+")
        # Double dot
        self.assertRaises(FlexlayError, semver.set_prerelease, "first..second")
        # Ends or starts with dot
        self.assertRaises(FlexlayError, semver.set_prerelease, "a.")
        self.assertRaises(FlexlayError, semver.set_prerelease, ".a")
        # Numbers, letters, dashes
        semver.set_prerelease("this.is-valid")
        self.assertEqual(semver.get_prerelease(), "this.is-valid")
        semver.set_prerelease("th1s-i5.4lso.valid.42")
        self.assertEqual(semver.get_prerelease(), "th1s-i5.4lso.valid.42")
        # None
        semver.set_prerelease(None)
        self.assertIs(semver.get_prerelease(), None)

    def test_build_metadata(self):
        semver = SemVer()
        # Check defaults to None
        self.assertIs(semver.get_build_metadata(), None)
        # None alphanumeric
        self.assertRaises(FlexlayError, semver.set_build_metadata, "a.@")
        self.assertRaises(FlexlayError, semver.set_build_metadata, "a.+")
        # Double dot
        self.assertRaises(FlexlayError, semver.set_build_metadata, "first..second")
        # Ends or starts with dot
        self.assertRaises(FlexlayError, semver.set_build_metadata, "a.")
        self.assertRaises(FlexlayError, semver.set_build_metadata, ".a")
        # Numbers, letters, dashes
        semver.set_build_metadata("this.is-valid")
        self.assertEqual(semver.get_build_metadata(), "this.is-valid")
        semver.set_build_metadata("th1s-i5.4lso.valid.42")
        self.assertEqual(semver.get_build_metadata(), "th1s-i5.4lso.valid.42")
        # None
        semver.set_build_metadata(None)
        self.assertIs(semver.get_build_metadata(), None)

    def test_to_str(self):
        semver = SemVer(3, 5, 4)
        self.assertEqual(str(semver), "3.5.4")
        semver2 = SemVer(0, 42, 43)
        self.assertEqual(str(semver2), "0.42.43")
        semver2.set_prerelease("this.string")
        self.assertEqual(str(semver2), "0.42.43-this.string")
        semver2.set_build_metadata("this.2nd.string")
        self.assertEqual(str(semver2), "0.42.43-this.string+this.2nd.string")

    def test_simple_comparisons(self):
        semver = SemVer(1, 2, 3)
        semver2 = SemVer(1, 2, 4)
        semver3 = SemVer(1, 3, 4)
        semver4 = SemVer(2, 0, 0)
        semver_equal = SemVer(1, 3, 4)

        self.assertTrue(semver < semver2, msg=str(semver) + " < " + str(semver2))
        self.assertTrue(semver2 > semver, msg=str(semver2) + " > " + str(semver))
        self.assertTrue(semver <= semver2, msg=str(semver) + " <= " + str(semver2))
        self.assertTrue(semver2 >= semver, msg=str(semver2) + " >= " + str(semver))
        self.assertTrue(semver != semver2, msg=str(semver) + " != " + str(semver2))
        self.assertTrue(semver2 < semver3, msg=str(semver2) + " < " + str(semver3))
        self.assertTrue(semver3 >= semver2, msg=str(semver3) + " >= " + str(semver2))
        self.assertTrue(semver4 > semver3, msg=str(semver4) + " > " + str(semver3))
        self.assertTrue(semver3 == semver_equal, msg=str(semver3) + " == " + str(semver_equal))

    def test_prerelease_and_metadata_comparisons(self):
        semver = SemVer(1, 4, 0)
        semver_equal = SemVer(1, 4, 0)

        # Prerelease makes semver less
        semver.set_prerelease("pre.release")

        self.assertTrue(semver < semver_equal, msg=str(semver) + " < " + str(semver_equal))
        self.assertFalse(semver == semver_equal, msg=str(semver) + " != " + str(semver_equal))

        # When both have pre-release data, they are indistinguishable
        semver_equal.set_prerelease("pre.release.different")
        self.assertTrue(semver == semver_equal, msg=str(semver) + " == " + str(semver_equal))
        self.assertFalse(semver_equal > semver, msg=str(semver_equal) + " <= " + str(semver))
        self.assertTrue(semver >= semver_equal, msg=str(semver) + " >= " + str(semver))

        # build_metadata means nothing
        semver.set_build_metadata("test.ing.build-metadata")
        self.assertFalse(semver != semver_equal, msg=str(semver) + " == " + str(semver_equal))
        self.assertFalse(semver_equal < semver, msg=str(semver_equal) + " >= " + str(semver))
        self.assertTrue(semver <= semver_equal, msg=str(semver) + " <= " + str(semver_equal))
