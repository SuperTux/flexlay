//  $Id: scripting.hxx,v 1.2 2003/09/10 10:58:29 grumbel Exp $
// 
//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef HEADER_SCRIPTING_HXX
#define HEADER_SCRIPTING_HXX

#include <guile/gh.h>

void editor_add_window(int x, int y, int w, int h, const char* title);
void editor_add_button(int x, int y, int w, int h, const char* text, SCM func);
void editor_add_label(int x, int y, const char* text);
void editor_add_inputbox(int x, int y, int w, int h, const char* text);
void editor_quit();

#endif

/* EOF */
