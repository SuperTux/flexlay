# This file was created automatically by SWIG.
# Don't modify this file, modify the SWIG interface instead.
# This file is compatible with both classic and new-style classes.

import _foo

def _swig_setattr(self,class_type,name,value):
    if (name == "this"):
        if isinstance(value, class_type):
            self.__dict__[name] = value.this
            if hasattr(value,"thisown"): self.__dict__["thisown"] = value.thisown
            del value.thisown
            return
    method = class_type.__swig_setmethods__.get(name,None)
    if method: return method(self,value)
    self.__dict__[name] = value

def _swig_getattr(self,class_type,name):
    method = class_type.__swig_getmethods__.get(name,None)
    if method: return method(self)
    raise AttributeError,name

import types
try:
    _object = types.ObjectType
    _newclass = 1
except AttributeError:
    class _object : pass
    _newclass = 0
del types


class Foo(_object):
    __swig_setmethods__ = {}
    __setattr__ = lambda self, name, value: _swig_setattr(self, Foo, name, value)
    __swig_getmethods__ = {}
    __getattr__ = lambda self, name: _swig_getattr(self, Foo, name)
    def __repr__(self):
        return "<C Foo instance at %s>" % (self.this,)
    def do_something(*args): return _foo.Foo_do_something(*args)
    def __init__(self, *args):
        _swig_setattr(self, Foo, 'this', _foo.new_Foo(*args))
        _swig_setattr(self, Foo, 'thisown', 1)
    def __del__(self, destroy=_foo.delete_Foo):
        try:
            if self.thisown: destroy(self)
        except: pass

class FooPtr(Foo):
    def __init__(self, this):
        _swig_setattr(self, Foo, 'this', this)
        if not hasattr(self,"thisown"): _swig_setattr(self, Foo, 'thisown', 0)
        _swig_setattr(self, Foo,self.__class__,Foo)
_foo.Foo_swigregister(FooPtr)


