#include <Python.h>
#include <ClanLib/Core/System/sharedptr.h>
#include <iostream>

struct Callback
{
  PyObject* o;

  Callback(PyObject* obj)
  {
    o = obj;
    Py_INCREF(o); 
    std::cout << "is callable: " << PyCallable_Check(obj) << std::endl;
  }

  void operator()()
  {
    std::cout << "Callback: " << o << std::endl;
    std::cout << "  Return: " << PyEval_CallObject(o,  PyTuple_New(0)) << std::endl;
  }
};

PyObject* callback;

void set_callback(PyObject* c)
{
  callback = c;
}

void call_callback()
{
  std::cout << "Calling callback" << std::endl;
  std::cout << PyEval_CallObject(callback,  PyTuple_New(0)) << std::endl;
}

BOOST_PYTHON_MODULE(hellopy)
{
  using namespace boost::python;

  def("setcallback",  &set_callback);
  def("callcallback", &call_callback);
}

/* EOF */

