//  $Id: gui_manager.hxx,v 1.2 2003/10/12 11:58:09 grumbel Exp $
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

#ifndef HEADER_GUI_MANAGER_HXX
#define HEADER_GUI_MANAGER_HXX

#include <stack>

class CL_GUIManager;
class CL_StyleManager;
class CL_ResourceManager;
class CL_SlotContainer;

/** */
class GUIManager
{
private:
  std::stack<CL_Component*> components;

  CL_GUIManager*      manager;
  CL_StyleManager*    style;
  CL_ResourceManager* resources;
  CL_SlotContainer*   slot_container;

  static GUIManager* current_;
public:
  static GUIManager* current() { return current_; }

  GUIManager();
  ~GUIManager();
  
  void draw();
  void update();

  void run();

  void push_component(CL_Component* c) { components.push(c); }
  void pop_component() { components.pop(); }

  void hide();
  void show();
  bool is_visible();

  CL_Component* get_component();  
  CL_SlotContainer* get_slot_container();
};

#endif

/* EOF */
