%module windstille

%include "std_vector.i"
//%include "std_string.i"
%template (vector_int) std::vector<int>;

%{
#include <string>
#include "editor.hxx"
#include "gui.hxx"
#include "netpanzer.hxx"
#include "../tile_brush.hxx"

#ifdef SWIGGUILE
TileBrush scm2brush(SCM s_brush);
SCM       brush2scm(const TileBrush& brush);
//std::string scm2string(SCM s);
//SCM string2scm(const std::string& str);

SCM
component2scm(CL_Component* comp) {
  return SWIG_NewPointerObj(comp, SWIGTYPE_p_CL_Component, 0);
}
#endif

%}

#ifdef SWIGGUILE
%typemap(in) SCMFunctor {
    $1 = SCMFunctor($input);
}

%typemap(out) SCMFunctor {
    $result = ($1).get_scm();
}

%typemap(in) TileBrush {
    $1 = scm2brush($input);
}

%typemap(out) TileBrush {
    $result = brush2scm($1);
}

%typemap(in) std::string {
      char* tmpstr = gh_scm2newstr($input, 0);
      $1 = tmpstr;
      free(tmpstr);
}

%typemap(out) std::string {
    $result = gh_str02scm($1.c_str());
}
#endif

%newobject tileset_create();

%include "editor.hxx"
%include "gui.hxx"
%include "netpanzer.hxx"

/* EOF */