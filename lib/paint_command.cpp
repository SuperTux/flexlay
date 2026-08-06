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
#include <map>
#include "paint_command.hpp"

struct CellChange
{
  int x;
  int y;
  int old_id;
  int new_id;
};

class PaintCommandImpl : public CommandImpl
{
public:
  typedef std::vector<CL_Point> Points;
  Points points;

  TilemapLayer tilemap;
  TileBrush    brush;

  /** Sparse per-cell undo/redo data (key = y * width + x). */
  std::map<int, CellChange> changes;

  PaintCommandImpl() {}
  virtual ~PaintCommandImpl() {}

  void execute();
  void redo();
  void undo();
  std::string serialize();

  void apply_brush_at(const CL_Point& pos);
};

PaintCommand::PaintCommand(TilemapLayer t, const TileBrush& b)
  : impl(new PaintCommandImpl())
{
  impl->tilemap = t;
  impl->brush   = b;
}

PaintCommand::~PaintCommand()
{
}

void
PaintCommandImpl::apply_brush_at(const CL_Point& pos)
{
  Field<int>* field = tilemap.get_field();
  if (!field)
    return;

  const int map_w = field->get_width();
  const int map_h = field->get_height();
  if (map_w <= 0 || map_h <= 0)
    return;

  for (int y = 0; y < brush.get_height(); ++y)
  {
    for (int x = 0; x < brush.get_width(); ++x)
    {
      const int tile_id = brush.at(x, y);
      if (!brush.is_opaque() && tile_id == 0)
        continue;

      const int tx = pos.x + x;
      const int ty = pos.y + y;
      if (tx < 0 || ty < 0 || tx >= map_w || ty >= map_h)
        continue;

      const int key = ty * map_w + tx;
      std::map<int, CellChange>::iterator it = changes.find(key);
      if (it == changes.end())
      {
        CellChange c;
        c.x = tx;
        c.y = ty;
        c.old_id = field->at(tx, ty);
        c.new_id = tile_id;
        changes.insert(std::make_pair(key, c));
      }
      else
      {
        it->second.new_id = tile_id;
      }
      field->at(tx, ty) = tile_id;
    }
  }
}

void
PaintCommand::add_point(const CL_Point& pos)
{
  if (!impl.get())
    return;
  impl->points.push_back(pos);
  impl->apply_brush_at(pos);
}

void
PaintCommandImpl::execute()
{
  // Painting already applied in add_point(); changes holds undo/redo data.
  if (points.empty() && changes.empty())
    return;
}

void
PaintCommandImpl::redo()
{
  Field<int>* field = tilemap.get_field();
  if (!field)
    return;
  for (std::map<int, CellChange>::iterator i = changes.begin(); i != changes.end(); ++i)
    field->at(i->second.x, i->second.y) = i->second.new_id;
}

void
PaintCommandImpl::undo()
{
  Field<int>* field = tilemap.get_field();
  if (!field)
    return;
  for (std::map<int, CellChange>::iterator i = changes.begin(); i != changes.end(); ++i)
    field->at(i->second.x, i->second.y) = i->second.old_id;
}

std::string
PaintCommandImpl::serialize()
{
  std::ostringstream s;
  s << "PaintCommand cells=" << changes.size()
    << " points=" << points.size() << std::endl;
  return s.str();
}

Command
PaintCommand::to_command()
{
  return Command(impl);
}

/* EOF */
