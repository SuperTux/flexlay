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

#include "gui/object_selector.hpp"

#include "gui/object_selector_widget.hpp"

class ObjectSelectorImpl
{
public:
  ObjectSelectorWidget* widget;
};

ObjectSelector::ObjectSelector(int obj_w, int obj_h, QWidget* parent) :
  m_impl(new ObjectSelectorImpl)
{
  m_impl->widget = new ObjectSelectorWidget(obj_w, obj_h, parent);
}

ObjectSelector::~ObjectSelector()
{
}

void
ObjectSelector::add_brush(const ObjectBrush& brush)
{
  m_impl->widget->add_brush(brush);
}

boost::signals2::signal<void (ObjectBrush, Point)>&
ObjectSelector::sig_drop()
{
  return m_impl->widget->sig_drop();
}

QWidget*
ObjectSelector::get_widget() const
{
  return m_impl->widget;
}

/* EOF */
