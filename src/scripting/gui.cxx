//  $Id: gui.cxx,v 1.3 2003/10/12 11:58:09 grumbel Exp $
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

#include <iostream>
#include <ClanLib/signals.h>
#include <ClanLib/display.h>
#include <ClanLib/gui.h>
#include <ClanLib/GUI/gui_manager.h>
#include "../gui_manager.hxx"
#include "gui.hxx"

#ifdef SWIGGUILE
#  include "scm_functor.hxx"
#endif

#ifdef SWIGGUILE
template<class A>
SCM any2scm(const A& a);

template<>
SCM any2scm<int>(const int& i){
  return gh_int2scm(i);
}

template <class A>
struct SCMGenericFunctor1
{
  SCMFunctor func;

  SCMGenericFunctor1(SCM f) 
    : func(f)
  {}

  void operator()(const A& a) {
    func(any2scm<A>(a));
  }
};

template <class A, class B>
struct SCMGenericFunctor2
{
  SCMFunctor func;

  SCMGenericFunctor2(SCM f) 
    : func(f)
  {}

  void operator()(const A& a, const B& b) {
    func(any2scm<A>(a),
         any2scm<B>(b));
  }
};

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


CL_MenuNode*
gui_add_menu_toggle_item(CL_Component* c_menu, const char* name, SCM func)
{
  CL_Menu* menu = dynamic_cast<CL_Menu*>(c_menu);

  if (menu)
    {
      CL_MenuNode* node = menu->create_toggle_item(name); 
      menu->reposition();
      //node->sig_clicked().connect_functor(SCMFunctor(func)).set_persistent();
      //std::cout << "Connecting menu function" << std::endl;
      new CL_Slot(node->sig_clicked().connect_functor(SCMFunctor(func)));
      return node;
    }
  else
    {
      std::cout << "Error: " << c_menu << " not a menu item" << std::endl;
      return 0;
    }
}

void
gui_add_menu_item(CL_Component* c_menu, const char* name, SCM func)
{
  CL_Menu* menu = dynamic_cast<CL_Menu*>(c_menu);

  if (menu)
    {
      CL_MenuNode* node = menu->create_item(name); 
      menu->reposition();
      //node->sig_clicked().connect_functor(SCMFunctor(func)).set_persistent();
      //std::cout << "Connecting menu function" << std::endl;
      new CL_Slot(node->sig_clicked().connect_functor(SCMFunctor(func)));
    }
  else
    {
      std::cout << "Error: " << c_menu << " not a menu item" << std::endl;
    }
}

CL_Component*
gui_create_button_func(int x, int y, int w, int h, const char* text, SCM func)
{
  CL_Component* comp = gui_create_button(x, y, w, h, text);
  gui_component_on_click(comp, func);
  return comp;
}

void
gui_file_dialog(const char* filename, SCM func)
{
  try {
    new CL_FileDialog("File Dialog", "/", "", GUIManager::current()->get_component());
  } catch (CL_Error& err) {
    std::cout << "CL_Error: " << err.message << std::endl;
  }
}

void
gui_listbox_on_click(CL_Component* box, SCM func)
{
  CL_ListBox* listbox = dynamic_cast<CL_ListBox*>(box);
  if (listbox)
    {
      new CL_Slot(listbox->sig_highlighted().connect_functor(SCMGenericFunctor1<int>(func)));
    }
}

void gui_add_on_resize_callback(SCM func)
{
  new CL_Slot(CL_Display::get_current_window()->sig_resize().
              connect_functor(SCMGenericFunctor2<int, int>(func)));
}

#endif
// ---------------------------------------------------------------

CL_Component*
gui_add_button(int x, int y, int w, int h, const char* text)
{
  CL_Component* manager = GUIManager::current()->get_component();
  return new CL_Button(CL_Rect(CL_Point(x, y), CL_Size(w, h)),
                       text, manager);
}

void
gui_component_set_position(CL_Component* comp, int x, int y)
{
  comp->set_position(x, y);
}

void
gui_component_set_rect(CL_Component* comp, int x, int y, int w, int h)
{
  comp->set_position(CL_Rect(CL_Point(x, y),
                             CL_Size(w, h)));
}


CL_Component*
gui_create_menu()
{
  CL_Component* manager = GUIManager::current()->get_component();
  CL_Menu* menu = new CL_Menu(manager);
  //menu->add_child(new CL_Label(CL_Point(250, 5), "Hello World", menu));
  return menu;
}

CL_Component*
gui_create_button(int x, int y, int w, int h, const char* text)
{
  CL_Component* manager = GUIManager::current()->get_component();
  return new CL_Button(CL_Rect(CL_Point(x, y), CL_Size(w, h)),
                       text, manager);
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

void gui_remove_component(CL_Component* comp)
{
  CL_Component* parent = comp->get_parent();
  parent->remove_child(comp);
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

void
gui_inputbox_set_text(CL_Component* comp, std::string txt)
{
  CL_InputBox* box = dynamic_cast<CL_InputBox*>(comp);
  if (box)
    box->set_text(txt);
}

std::string 
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
gui_quit()
{
  GUIManager::current()->get_component()->quit();
}

void gui_hide()
{
  GUIManager::current()->hide();
}

void gui_show()
{
  GUIManager::current()->show();
}

bool gui_is_visible()
{
  return GUIManager::current()->is_visible();
}

CL_Component*
gui_listbox_create(int x, int y, int w, int h)
{
  return new CL_ListBox(CL_Rect(CL_Point(x, y),
                                CL_Size(w, h)),
                                GUIManager::current()->get_component());
}

int
gui_listbox_add(CL_Component* box, const char* str)
{
  CL_ListBox* listbox = dynamic_cast<CL_ListBox*>(box);

  if (listbox)
    return listbox->insert_item(str);
  else
    return -1;
}


int gui_component_get_width(CL_Component* comp)
{
  return comp->get_width();
}

int gui_component_get_height(CL_Component* comp)
{
  return comp->get_height();
}

/* EOF */
