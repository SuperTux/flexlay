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

#include "paint_command.hpp"

#include <sstream>

#include "math/rect.hpp"

class PaintCommandImpl : public CommandImpl
{
public:
  typedef std::vector<Point> Points;
  Points points;

  TilemapLayer tilemap;
  TileBrush    brush;

  /** Copy of the field used to generate undo informations */
  Field<int>   undo_field;

  Point     pos;
  TileBrush*   redo_brush;
  TileBrush*   undo_brush;

  PaintCommandImpl() {}
  virtual ~PaintCommandImpl() {}

  void execute();

  void redo();
  void undo();

  std::string serialize();
};

PaintCommand::PaintCommand(TilemapLayer t, const TileBrush& b)
  : impl(new PaintCommandImpl())
{
  impl->tilemap = t;
  impl->brush   = b;

  impl->undo_field = *(impl->tilemap.get_field());

  impl->redo_brush = 0;
  impl->undo_brush = 0;
}

PaintCommand::~PaintCommand()
{
  delete impl->redo_brush;
  delete impl->undo_brush;
}

void
PaintCommand::add_point(const Point& pos)
{
  // FIXME: undo_field is unneeded, should just record the overwritten color
  impl->points.push_back(pos);
  impl->tilemap.draw_tile(impl->brush, pos);
}

void
PaintCommandImpl::execute()
{
  assert(!points.empty());

  // Calc bounding rect
  Rect rect(points.front().x,
               points.front().y,
               points.front().x + brush.get_width(),
               points.front().y + brush.get_height());

  for(PaintCommandImpl::Points::iterator i = points.begin(); i != points.end(); ++i)
  {
    rect.left   = std::min(rect.left,   (*i).x);
    rect.top    = std::min(rect.top,    (*i).y);
    rect.right  = std::max(rect.right,  (*i).x + brush.get_width());
    rect.bottom = std::max(rect.bottom, (*i).y + brush.get_height());
  }

  pos.x = rect.left;
  pos.y = rect.top;

  redo_brush = new TileBrush(*(tilemap.get_field()), rect.get_width(), rect.get_height(),
                             -pos.x, -pos.y);
  // FIXME: undo_field is unneeded, should just record the overwritten color
  undo_brush = new TileBrush(undo_field, rect.get_width(), rect.get_height(),
                             -pos.x, -pos.y);

  redo_brush->set_opaque();
  undo_brush->set_opaque();

  undo_field.clear();
}

void
PaintCommandImpl::redo()
{
  TilemapLayer::draw_tiles(tilemap.get_field(), *redo_brush, pos);
}

void
PaintCommandImpl::undo()
{
  TilemapLayer::draw_tiles(tilemap.get_field(), *undo_brush, pos);
}

std::string
PaintCommandImpl::serialize()
{
  std::stringstream s;

  s << "_ = PaintCommand(" << &tilemap << ", " << &brush << ")" << std::endl;
  for(PaintCommandImpl::Points::iterator i = points.begin(); i != points.end(); ++i)
  {
    s << "_.add_paint(" << i->x << ", " << i->y << ")"  << std::endl;
  }
  s << "_ = None" << std::endl;

  return s.str();
}

Command
PaintCommand::to_command()
{
  return Command(impl);
}

/* EOF */
