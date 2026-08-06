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

#include <sstream>
#include <memory>
#include "paint_command.hpp"

class PaintCommandImpl : public CommandImpl
{
public:
  typedef std::vector<CL_Point> Points;
  Points points;

  TilemapLayer tilemap;
  TileBrush    brush;

  /** Copy of the field used to generate undo informations */
  Field<int>   undo_field;

  CL_Point     pos;
  std::unique_ptr<TileBrush> redo_brush;
  std::unique_ptr<TileBrush> undo_brush;

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

  Field<int>* field = impl->tilemap.get_field();
  if (field)
    impl->undo_field = *field;
}

PaintCommand::~PaintCommand()
{
}

void
PaintCommand::add_point(const CL_Point& pos)
{
  if (!impl.get())
    return;
  impl->points.push_back(pos);
  impl->tilemap.draw_tile(impl->brush, pos);
}

void
PaintCommandImpl::execute()
{
  if (points.empty())
    return;

  // Calc bounding rect
  CL_Rect rect(points.front().x,
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

  Field<int>* field = tilemap.get_field();
  if (!field)
    return;

  redo_brush.reset(new TileBrush(*field, rect.get_width(), rect.get_height(),
                                 -pos.x, -pos.y));
  // Full-field snapshot is heavy; differential undo would be better later.
  undo_brush.reset(new TileBrush(undo_field, rect.get_width(), rect.get_height(),
                                 -pos.x, -pos.y));

  redo_brush->set_opaque();
  undo_brush->set_opaque();

  undo_field.clear();
}

void
PaintCommandImpl::redo()
{
  if (!redo_brush.get())
    return;
  TilemapLayer::draw_tiles(tilemap.get_field(), *redo_brush, pos);
}

void
PaintCommandImpl::undo()
{
  if (!undo_brush.get())
    return;
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
