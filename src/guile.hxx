//  $Id: guile.hxx,v 1.13 2003/06/22 17:22:47 grumbel Exp $
// 
//  Feuerkraft - A Tank Battle Game
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#ifndef GUILE_HXX
#define GUILE_HXX

#include <libguile.h>
#include <string>
#include <iostream>

/** A loose collection of Guile helper functions */
namespace Guile {

/** Convert a scheme string into a C++ std::string, converting other
    non string SCM data isn't currently supported */
std::string scm2string (SCM data);

/** Used in the BuildingMap: x,y => (pos x y) */
SCM pos2scm (int x, int y);

SCM symbol_value_pair (const std::string&, float);

/** Pretty print the object given by obj */
void pretty_print (std::ostream& s, SCM obj);

bool equal_p(SCM a, SCM b);

SCM symbol2scm(const char* str);

std::string keyword2string(SCM keyword);

std::string symbol2string(SCM symbol);

/** Enters the read-eval-print-loop, which can be exited by (q) */
void enter_repl();

/** Switches on debugging for Guile, causing line numbers and
    backtraces be printed on errors */
void enable_debug();

/** Disable all debugging */
void disable_debug();

void enable_readline();

} // namespace Guile

#endif

/* EOF */
