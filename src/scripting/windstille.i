%module windstille
%{
#include <string>
#include "editor.hxx"
#include "game.hxx"
#include "gui.hxx"

TileBrush scm2brush(SCM s_brush);
SCM       brush2scm(const TileBrush& brush);
std::string scm2string(SCM s);
SCM string2scm(const std::string& str);
%}

%typemap(in) TileBrush {
    $1 = scm2brush($input);
}

%typemap(out) TileBrush {
    $result = brush2scm($1);
}

%typemap(in) std::string {
    $1 = scm2string($input);
}

%typemap(out) std::string {
    $result = string2scm($1);
}

%include "editor.hxx"
%include "game.hxx"
%include "gui.hxx"

/* EOF */