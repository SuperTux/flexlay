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

#include <iostream>

#include "helper.hpp"
#include "gui/icon.hpp"

ButtonPanel::ButtonPanel(const Rect& rect, bool horizontal) :
  m_horizontal(horizontal)
{
  std::cout << "ButtonPanel in C++" << std::endl;
}

Icon*
ButtonPanel::add_small_icon(const std::string& image,
                            std::function<void ()> callback)
{
#ifdef GRUMBEL
  std::string tooltip;
  Icon* icon = nullptr;
  if (m_horizontal)
  {
    icon = new Icon(Rect(Point(m_pos,  2), Size(16, 32)),
                    make_sprite(image), tooltip, m_panel);
  }
  else
  {
    icon = new Icon(Rect(Point(2, m_pos), Size(16, 32)),
                    make_sprite(image), tooltip, m_panel);
  }

  m_pos += 16;

  if (callback)
  {
    icon->sig_clicked().connect(callback);
  }

  return icon;
#endif
  return nullptr;
}

Icon*
ButtonPanel::add_icon(const std::string& image,
                      std::function<void ()> callback)
{
#ifdef GRUMBEL
  std::string tooltip;
  Icon* icon = nullptr;
  if (m_horizontal)
  {
    icon = new Icon(Rect(Point(m_pos,  2), Size(32, 32)),
                    make_sprite(image), tooltip, m_panel);
  }
  else
  {
    icon = new Icon(Rect(Point(2, m_pos), Size(32, 32)),
                    make_sprite(image), tooltip, m_panel);
  }

  m_pos += 32;

  if (callback)
  {
    icon->sig_clicked().connect(callback);
  }

  return icon;
#endif
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
