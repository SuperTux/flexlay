%module windstille
%{
#include "editor.hxx"
#include "game.hxx"
#include "gui.hxx"

TileBrush scm2brush(SCM s_brush);
SCM brush2scm(const TileBrush& brush);
%} 

%typemap(in) TileBrush {
    $1 = scm2brush($input);
}

%typemap(out) TileBrush {
    $result = brush2scm($1);
}

%include "editor.hxx"
%include "game.hxx"
%include "gui.hxx"

/* EOF */