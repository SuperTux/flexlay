//  $Id: gui.cxx,v 1.2 2003/10/11 08:11:59 grumbel Exp $
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

#include <ClanLib/signals.h>
#include <ClanLib/gui.h>
#include "scm_functor.hxx"
#include "../gui_manager.hxx"
#include "gui.hxx"

struct SCMVirtualFunctor
{
  SCMFunctor func;

  SCMVirtualFunctor(const SCMFunctor& f)
    : func(f)
  {}

  void operator()(CL_SlotParent_v0& parent) {
    func();
  }
};

CL_Component*
gui_add_button(int x, int y, int w, int h, const char* text)
{
  CL_Component* manager = GUIManager::current()->get_component();
  return new CL_Button(CL_Rect(CL_Point(x, y), CL_Size(w, h)),
                       text, manager);
}

void
gui_component_on_click(CL_Component* comp, SCM func)
{
  CL_Button* button = dynamic_cast<CL_Button*>(comp);
  CL_SlotContainer* slot_container = GUIManager::current()->get_slot_container();
  
  slot_container->connect_functor(button->sig_clicked(), SCMFunctor(func));
}

void
gui_component_on_close(CL_Component* comp, SCM func)
{
  CL_Window* window = dynamic_cast<CL_Window*>(comp);
  // FIXME: Slot container considered harmfull
  //CL_SlotContainer* slot_container = GUIManager::current()->get_slot_container();
  new CL_Slot(window->sig_close().connect_functor_virtual(SCMVirtualFunctor(SCMFunctor(func))));
}

CL_Component*
gui_create_button(int x, int y, int w, int h, const char* text)
{
  CL_Component* manager = GUIManager::current()->get_component();
  return new CL_Button(CL_Rect(CL_Point(x, y), CL_Size(w, h)),
                       text, manager);
}

CL_Component*
gui_create_button_func(int x, int y, int w, int h, const char* text, SCM func)
{
  CL_Component* comp = gui_create_button(x, y, w, h, text);
  gui_component_on_click(comp, func);
  return comp;
}

CL_Component* 
gui_create_label(int x, int y, const char* text)
{
  CL_Component* manager = GUIManager::current()->get_component();
  return new CL_Label(CL_Point(x, y), text, manager);
}

CL_Component*
gui_create_window(int x, int y, int w, int h, const char* title)
{
  CL_Component* manager = GUIManager::current()->get_component();
  return new CL_Window(CL_Rect(CL_Point(x, y), CL_Size(w, h)), title, manager);
}

CL_Component*
gui_create_inputbox(int x, int y, int w, int h, const char* text)
{
  CL_Component* manager = GUIManager::current()->get_component();
  return new CL_InputBox(CL_Rect(CL_Point(x,y), CL_Size(w, h)), text, manager);
}


CL_Component*
gui_push_component(CL_Component* c)
{
  GUIManager::current()->push_component(c);
  return c;
}

void
gui_pop_component()
{
  GUIManager::current()->pop_component();
}


void 
gui_window_close(CL_Component* comp)
{
  comp->close();
}

const char* 
gui_inputbox_get_text(CL_Component* comp)
{
  CL_InputBox* box = dynamic_cast<CL_InputBox*>(comp);
  if (box)
    {
      return box->get_text().c_str();
    }
  else
    {
      return 0;
    }
}

void
gui_hide_component(CL_Component* comp)
{
  comp->show(false);
}

void
gui_show_component(CL_Component* comp)
{
  comp->show(true);
}

bool
gui_component_is_visible(CL_Component* comp)
{
  return comp->is_visible();
}

CL_Component* 
gui_window_get_client_area(CL_Component* comp)
{
  return comp->get_client_area();
}

void
gui_file_dialog(const char* filename, SCM func)
{
  new CL_FileDialog("File Dialog", "/", "", GUIManager::current()->get_component());
}

void
gui_quit()
{
  GUIManager::current()->get_component()->quit();
}

/* EOF */
