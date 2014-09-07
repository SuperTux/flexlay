// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
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

#include "gui/button_panel.hpp"

#include <QAction>
#include <QToolBar>

#include <iostream>

#include "helper.hpp"
#include "gui/icon.hpp"

ButtonPanel::ButtonPanel(QToolBar* toolbar) :
  m_toolbar(toolbar)
{
  std::cout << "ButtonPanel in C++" << std::endl;
}

Icon*
ButtonPanel::add_icon(const std::string& name,
                      std::function<void ()> callback)
{
  QAction* action = m_toolbar->addAction(QString::fromStdString(name));
  if (callback)
  {
    QObject::connect(action, &QAction::triggered, callback);
  }

  return nullptr;
}

void
ButtonPanel::add_separator()
{
}

void
ButtonPanel::show(bool visible)
{
}

/* EOF */
