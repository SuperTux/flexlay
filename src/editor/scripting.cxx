//  $Id: scripting.cxx,v 1.3 2003/09/10 13:53:11 grumbel Exp $
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

#include <ClanLib/gui.h>
#include "../scm_functor.hxx"
#include "editor.hxx"
#include "editor_tilemap.hxx"
#include "scripting.hxx"

void
editor_set_brush_tile(int i)
{
  Editor::current()->get_editor_tilemap()->brush_tile = i;
}

int
editor_get_brush_tile()
{
  return Editor::current()->get_editor_tilemap()->brush_tile;
}

void
editor_add_button(int x, int y, int w, int h, const char* text, SCM func)
{
  CL_Component* manager = Editor::current()->get_component();
  CL_SlotContainer* slot_container = Editor::current()->get_slot_container();
 
  CL_Button* button = new CL_Button(CL_Rect(CL_Point(x, y), CL_Size(w, h)),
                                    text, manager);
  SCMFunctor* functor = new SCMFunctor(func);
  slot_container->connect(button->sig_clicked(), functor, &SCMFunctor::call);
}

void
editor_add_label(int x, int y, const char* text)
{
  CL_Component* manager = Editor::current()->get_component();
  CL_SlotContainer* slot_container = Editor::current()->get_slot_container();

  new CL_Label(CL_Point(x, y), text, manager);
}

void
editor_add_window(int x, int y, int w, int h, const char* title)
{
  CL_Component* manager = Editor::current()->get_component();
  CL_SlotContainer* slot_container = Editor::current()->get_slot_container();

  Editor::current()->set_component((new CL_Window(CL_Rect(CL_Point(x, y), CL_Size(w, h)), title, manager))->get_client_area());
}

void
editor_add_inputbox(int x, int y, int w, int h, const char* text)
{
  CL_Component* manager = Editor::current()->get_component();
  new CL_InputBox(CL_Rect(CL_Point(x,y), CL_Size(w, h)), text, manager);
}

void
editor_quit()
{
  Editor::current()->get_component()->quit();
}

/* EOF */
