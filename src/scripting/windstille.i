%module windstille
%{
#include <string>
#include "game.hxx"

std::string scm2string(SCM s);
SCM string2scm(const std::string& str);

%}

%typemap(in) SCMFunctor {
    $1 = SCMFunctor($input);
}

%typemap(out) SCMFunctor {
    $result = ($1).get_scm();
}

%typemap(in) std::string {
    $1 = scm2string($input);
}

%typemap(out) std::string {
    $result = string2scm($1);
}

%include "game.hxx"

/* EOF */