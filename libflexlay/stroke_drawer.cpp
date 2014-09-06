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

#include "stroke_drawer_impl.hpp"
#include "stroke_drawer.hpp"

StrokeDrawer::StrokeDrawer()
{
}

StrokeDrawer::StrokeDrawer(std::shared_ptr<StrokeDrawerImpl> impl_)
  :impl(impl_)
{

}

void
StrokeDrawer::draw(const Stroke& stroke, GraphicContext& gc)
{
  if (impl.get() != 0)
    impl->draw(stroke, gc);
}

StrokeDrawer
StrokeDrawer::clone() const
{
  return StrokeDrawer(std::shared_ptr<StrokeDrawerImpl>(impl->clone()));
}

/* EOF */
