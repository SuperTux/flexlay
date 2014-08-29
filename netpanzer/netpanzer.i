%module netpanzer_wrap
%include "std_string.i"

%{
#include <iostream>
#include <ClanLib/Core/System/error.h>
#include <ClanLib/Display/sprite.h>
#include "netpanzer.hpp"
%}

%import  "../lib/flexlay_wrap.i"
%include "netpanzer.hpp"


// EOF //
