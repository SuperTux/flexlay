//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_GUI_MANAGER_HPP
#define HEADER_FLEXLAY_GUI_MANAGER_HPP

#include <memory>

class GUIManagerImpl;

class GUIManager
{
private:
  static GUIManager* current_;

public:
  static GUIManager* current() { return current_; }

  GUIManager();
  ~GUIManager();

  void draw();
  void update();

  void run();
  void quit();

  void push_component(CL_Component* c);
  void pop_component();

  void hide();
  void show();
  bool is_visible();

  CL_Component* get_component();  
  CL_SlotContainer* get_slot_container();

private:
  std::shared_ptr<GUIManagerImpl> impl;
};

#endif

/* EOF */
