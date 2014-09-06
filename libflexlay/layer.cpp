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

#include "layer.hpp"

#include "graphic_context.hpp"
#include "layer_impl.hpp"

Layer::Layer() :
  impl()
{
}

Layer::Layer(std::shared_ptr<LayerImpl> i)
  : impl(i)
{
}

Layer::~Layer()
{
}

void
Layer::draw(GraphicContext& gc)
{
  if (impl.get())
  {
    if (impl->pos.x != 0 || impl->pos.y != 0)
    {
      gc.push_modelview();
      gc.add_translate(impl->pos.x, impl->pos.y);
      impl->draw(gc);
      gc.pop_modelview();
    }
    else
    {
      impl->draw(gc);
    }
  }
}

bool
Layer::has_bounding_rect() const
{
  if (impl.get())
    return impl->has_bounding_rect();
  else
    return false;
}

Rect
Layer::get_bounding_rect()
{
  Rect rect;

  if (impl.get())
  {
    rect = impl->get_bounding_rect();
    rect.left   += static_cast<int>(impl->pos.x);
    rect.top    += static_cast<int>(impl->pos.y);
    rect.right  += static_cast<int>(impl->pos.x);
    rect.bottom += static_cast<int>(impl->pos.y);
  }

  return rect;
}

MetaData
Layer::get_metadata() const
{
  if (impl.get())
    return impl->data;
  else
    return MetaData();
}

void
Layer::set_metadata(MetaData data_)
{
  if (impl.get())
    impl->data = data_;
}

void
Layer::set_pos(const Pointf& pos)
{
  impl->pos = pos;
}

Pointf
Layer::get_pos() const
{
  return impl->pos;
}

bool
Layer::is_null() const
{
  return impl.get() == 0;
}

/* EOF */
