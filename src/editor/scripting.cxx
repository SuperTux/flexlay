//  $Id: scripting.cxx,v 1.1 2003/09/10 08:25:29 grumbel Exp $
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
#include "scripting.hxx"

void
editor_add_button(int x, int y, int w, int h, const char* text, SCM func)
{
  CL_GUIManager* manager = Editor::current()->get_gui_manager();
  CL_SlotContainer* slot_container = Editor::current()->get_slot_container();
 
  CL_Button* button = new CL_Button(CL_Rect(CL_Point(x, y), CL_Size(w, h)),
                                    text, manager);
  manager->add_child(button);

  SCMFunctor* functor = new SCMFunctor(func);
  slot_container->connect(button->sig_clicked(), functor, &SCMFunctor::call);
}

/* EOF */
