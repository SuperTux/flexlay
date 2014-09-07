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

#include "gui/menubar.hpp"

#include <QMenu>
#include <QMenuBar>

#include "math/rect.hpp"

Menu::Menu(QMenu* menu) :
  m_menu(menu)
{  
}

Menu
Menu::add_menu(const std::string& label)
{
  QMenu* menu = m_menu->addMenu(QString::fromStdString(label));
  return Menu(menu);
}

void
Menu::add_item(const std::string& label, std::function<void()> callback)
{
  QAction* action = m_menu->addAction(QString::fromStdString(label));
  if (callback)
  {
    QObject::connect(action, &QAction::triggered, callback);
  }
}


Menubar::Menubar(QMenuBar* menubar) :
  m_menubar(menubar)
{  
}

Menu
Menubar::add_menu(const std::string& label)
{
  QMenu* menu = m_menubar->addMenu(QString::fromStdString(label));
  return Menu(menu);
}

/* EOF */

