//  $Id: editor.cxx,v 1.1 2003/09/10 08:25:29 grumbel Exp $
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
#include <ClanLib/guistyleboring.h>
#include <guile/gh.h>
#include "../globals.hxx"
#include "editor.hxx"

extern "C" void SWIG_init(void);

Editor* Editor::current_ = 0;

class MyComponent : public CL_Component
{
private:
  CL_SlotContainer slots;
public:
  MyComponent(const CL_Rect& pos, CL_Component* parent)
    : CL_Component(pos, parent)
  {
    slots.connect(sig_paint(), this, &MyComponent::paint);
  }

  void paint() 
  {
    CL_Display::clear(CL_Color::red);
  }
};

static void some_func()
{
  std::cout << "QUIT" << std::endl;
}

Editor::Editor()
{
  current_ = this;

  SWIG_init();

  slot_container = new CL_SlotContainer();
  resources = new CL_ResourceManager(datadir + "gui/gui.xml", false);
  style     = new CL_StyleManager_Boring(resources);
  manager   = new CL_GUIManager(style);

  manager->add_child(new CL_Button(CL_Rect(CL_Point(100, 100), CL_Size(150, 20)),
                                   "Hello World", manager));

  manager->add_child(new MyComponent(CL_Rect(CL_Point(100, 200), CL_Size(150, 20)), manager));

  CL_Button* button = new CL_Button(CL_Rect(CL_Point(100, 130), CL_Size(150, 20)),
                                    "Quit", manager);
  manager->add_child(button);
  slot_container->connect(button->sig_clicked(), this, &Editor::popup_menu);

  popupmenu = new CL_PopupMenu(manager);
  menu_data  = new CL_MenuData(popupmenu);
  menu_data->insert_item("Hello World");
  menu_data->insert_item("Hello World2");
  menu_data->insert_item("Hello World3");

  gh_load ((datadir + "editor.scm").c_str());
}

void
Editor::popup_menu()
{
  std::cout << "PopUP" << std::endl;
  popupmenu->popup(manager);
}

Editor::~Editor()
{
  delete manager;
  delete style;
  delete resources;
  delete slot_container;
}

void
Editor::run()
{
  manager->run();
}

/* EOF */
