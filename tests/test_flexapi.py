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

import sys; sys.path.append("..")
import unittest

from flexapi.tests.test_semver import SemVerTestCase
from flexapi.tests.test_signal import SignalTestCase
from flexapi.tests.test_text_iterator import TextIteratorTestCase
from flexapi.tests.test_vector import VectorTestCase

if __name__ == "__main__":
    # RUN ALL THE TESTS
    suite = unittest.TestSuite()
    suite.addTest(unittest.TestLoader().loadTestsFromTestCase(SemVerTestCase))
    suite.addTest(unittest.TestLoader().loadTestsFromTestCase(SignalTestCase))
    suite.addTest(unittest.TestLoader().loadTestsFromTestCase(TextIteratorTestCase))
    suite.addTest(unittest.TestLoader().loadTestsFromTestCase(VectorTestCase))
    unittest.TextTestRunner(verbosity=2).run(suite)

