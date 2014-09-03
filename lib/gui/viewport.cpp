//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "graphic_context_state.hpp"
#include "viewport.hpp"

class ViewportImpl
{
public:
  CL_Component* child;
  GraphicContextState gc_state;
};

Viewport::Viewport(CL_Component* child, const Rect& rect, CL_Component* parent)
  : CL_Component(rect.to_cl(), parent),
    impl(new ViewportImpl())
{
  impl->child = child;
  impl->gc_state.set_size(rect.get_width(), rect.get_height());
}

void
Viewport::set_pos(const Pointf& pos)
{
  impl->gc_state.set_pos(pos);
}

Pointf
Viewport::get_pos() const
{
  return impl->gc_state.get_pos();
}

/* EOF */
