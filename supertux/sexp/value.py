# SExp - A S-Expression Parser for Python
# Copyright (c) 2015 Ingo Ruhnke <grumbel@gmail.com>
#
# This software is provided 'as-is', without any express or implied
# warranty. In no event will the authors be held liable for any damages
# arising from the use of this software.
#
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
#
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
# 3. This notice may not be removed or altered from any source distribution.


class Value:

    def __init__(self, pos):
        self.pos = pos

    def is_nil(self):
        return False

    def is_boolean(self):
        return False

    def is_integer(self):
        return False

    def is_real(self):
        return False

    def is_cons(self):
        return False

    def is_string(self):
        return False

    def is_symbol(self):
        return False

    def is_array(self):
        return False

    def line(self):
        return self.pos[0]

    def column(self):
        return self.pos[1]


class Nil(Value):

    def __init__(self, pos=None):
        super().__init__(pos)

    def is_nil(self):
        return True

    def __eq__(self, other):
        return other.is_nil()

    def __str__(self):
        return "()"


class Boolean(Value):

    def __init__(self, v, pos=None):
        super().__init__(pos)
        self.value = v

    def is_boolean(self):
        return True

    def __eq__(self, other):
        return other.is_boolean() and self.value == other.value

    def __str__(self):
        if self.value:
            return "#t"
        else:
            return "#f"


class Integer(Value):

    def __init__(self, v, pos=None):
        super().__init__(pos)
        self.value = v

    def is_integer(self):
        return True

    def __eq__(self, other):
        return other.is_integer() and self.value == other.value

    def __str__(self):
        return str(self.value)


class Real(Value):

    def __init__(self, v, pos=None):
        super().__init__(pos)
        self.value = v

    def is_real(self):
        return True

    def __eq__(self, other):
        return other.is_real() and self.value == other.value

    def __str__(self):
        return str(self.value)


class String(Value):

    def __init__(self, v, pos=None):
        super().__init__(pos)
        self.value = v

    def is_string(self):
        return True

    def __eq__(self, other):
        return other.is_string() and self.value == other.value

    def __str__(self):
        return str(self.value)


class Symbol(Value):

    def __init__(self, v, pos=None):
        super().__init__(pos)
        self.value = v

    def is_symbol(self):
        return True

    def __eq__(self, other):
        return other.is_symbol() and self.value == other.value

    def __str__(self):
        return str(self.value)


class Cons(Value):

    def __init__(self, car, cdr, pos=None):
        super().__init__(pos)
        self.car = car
        self.cdr = cdr

    def is_cons(self):
        return True

    def get_car(self):
        return self.car

    def get_cdr(self):
        return self.cdr

    def __eq__(self, other):
        return other.is_cons() and self.car == other.car and self.cdr == other.cdr

    def __str__(self):
        result = "("

        cur = self
        while not cur.is_nil():
            result += str(cur.car)
            if cur.cdr.is_cons():
                result += " "
                cur = cur.cdr
            elif cur.cdr.is_nil():
                break
            else:
                result += " . " + str(cur.cdr)
                break

        result += ")"
        return result


class Array(Value):

    def __init__(self, values, pos=None):
        self.values = values

    def is_array(self):
        return True

    def __eq__(self, other):
        return other.is_array() and self.values == other.values

    def __str__(self):
        return "#(" + " ".join([str(v) for v in self.values]) + ")"


def make_list(*values):
    if not values:
        return Nil()
    else:
        return Cons(values[0], make_list(*values[1:]))


# EOF #
