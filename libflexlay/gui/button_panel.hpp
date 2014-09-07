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

#ifndef HEADER_BUTTON_PANEL_HPP
#define HEADER_BUTTON_PANEL_HPP

#include <string>
#include <functional>

class QToolBar;
class Icon;
class Rect;

class ButtonPanel
{
private:
  QToolBar* m_toolbar;

public:
  ButtonPanel(QToolBar* toolbar);

  Icon* add_icon(const std::string& filename,
                 std::function<void ()> callback);
  Icon* add_text(const std::string& text,
                 std::function<void ()> callback);
  void add_separator();

  void show(bool visible);

private:
  ButtonPanel(const ButtonPanel&);
  ButtonPanel& operator=(const ButtonPanel&);
};

#endif

/* EOF */
