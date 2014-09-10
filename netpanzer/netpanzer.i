%module netpanzer_wrap
%include "std_string.i"

%{
#include <iostream>
#include "netpanzer.hpp"
%}

%import  "../libflexlay/flexlay_wrap.i"
%include "netpanzer.hpp"

// EOF //
