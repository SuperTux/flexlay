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

Menubar::Menubar(QMenuBar* menubar) :
  m_menubar(menubar)
{  
}

void
Menubar::add_item(const std::string& path, std::function<void()> callback)
{
  QMenu* menu = m_menubar->addMenu(QString::fromStdString(path));
  menu->addAction(QString::fromStdString(path));
  //m_slots.push_back(item->sig_clicked().connect_functor(callback));
}

/* EOF */

