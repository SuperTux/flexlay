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

#include "input_event.hpp"

#include <QMouseEvent>

#include <iostream>

InputEvent::InputEvent() :
  id(),
  mouse_pos(),
  mod()
{
}

InputEvent::InputEvent(QMouseEvent& event) :
  id(),
  mouse_pos(event.x(), event.y()),
  mod()
{
  if (event.button() == 0)
  {
    id = MOUSE_NO_BUTTON;
  }
  else if (event.button() == Qt::LeftButton)
  {
    id = MOUSE_LEFT;
  }
  else if (event.button() == Qt::MidButton)
  {
    id = MOUSE_MIDDLE;
  }
  else if (event.button() == Qt::RightButton)
  {
    id = MOUSE_RIGHT;
  }
  else
  {
    std::cout << "unknown mouse button: " << event.button() << std::endl;
  }
}

/* EOF */
