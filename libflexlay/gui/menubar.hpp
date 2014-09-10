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

class Menu;
class MenubarImpl;
class Point;
class QMenuBar;
class QMenu;

class Menu
{
private:
  QMenu* m_menu;

public:
#ifndef SWIG
  Menu(QMenu* menu);
#endif

public:
  Menu add_menu(const std::string& label);
  void add_item(const std::string& label, std::function<void()> callback);

#ifdef SWIG
private:
  Menu(const Menu&);
  Menu& operator=(const Menu&);
#endif
};

class Menubar
{
private:
  QMenuBar* m_menubar;

public:
#ifndef SWIG
  Menubar(QMenuBar* menubar);
  ~Menubar() {}
#endif

  Menu add_menu(const std::string& label);

private:
  Menubar(const Menubar&);
  Menubar& operator=(const Menubar&);
};

#endif

/* EOF */
