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

#include "icon.hpp"

#include <QAction>

#include "display.hpp"
#include "math/rect.hpp"
#include "box.hpp"

class IconImpl
{
public:
  QAction* action;
};

Icon::Icon(QAction* action) :
  m_impl(new IconImpl)
{
  m_impl->action = action;
}

void
Icon::disable()
{
  m_impl->action->setEnabled(false);
}

void
Icon::enable()
{
  m_impl->action->setEnabled(true);
}

void
Icon::set_up()
{
  m_impl->action->setCheckable(true);
  m_impl->action->setChecked(false);
}

void
Icon::set_down()
{
  m_impl->action->setCheckable(true);
  m_impl->action->setChecked(true);
}

/* EOF */
