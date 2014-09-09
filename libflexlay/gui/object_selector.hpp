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

#ifndef HEADER_FLEXLAY_OBJECT_SELECTOR_HPP
#define HEADER_FLEXLAY_OBJECT_SELECTOR_HPP

#include "../object_brush.hpp"

#include <QWidget>

class ObjectSelectorImpl;
class QWidget;

class ObjectSelector
{
public:
#ifndef SWIG
  ObjectSelector(int obj_w, int obj_h, QWidget* parent);
  ~ObjectSelector();

  QWidget* get_widget() const;
#endif

public:
  void add_brush(const ObjectBrush& brush);

  boost::signals2::signal<void (ObjectBrush, Point)>& sig_drop();

private:
  std::shared_ptr<ObjectSelectorImpl> m_impl;

private:
  ObjectSelector(const ObjectSelector&);
  ObjectSelector& operator=(const ObjectSelector&);
};

#endif

/* EOF */
