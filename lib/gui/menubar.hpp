// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_MENUBAR_HPP
#define HEADER_FLEXLAY_MENUBAR_HPP

#include <memory>
#include <vector>

#include <ClanLib/Signals/slot.h>

class Menu;
class MenubarImpl;
class Point;
class CL_Menu;
class CL_Component;

class Menubar
{
private:
  CL_Menu* m_menu;
  std::vector<CL_Slot> m_slots;

protected:
  virtual ~Menubar() {}
public:
  Menubar(CL_Component* parent);

  void add_item(const std::string& path, std::function<void()> callback);

private:
  std::shared_ptr<MenubarImpl> impl;
};

#endif

/* EOF */
