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

#ifndef HEADER_FLEXLAY_LAYER_HPP
#define HEADER_FLEXLAY_LAYER_HPP

#include <memory>

#include "meta_data.hpp"

class CL_GraphicContext;
class GraphicContext;
class GraphicContextState;
class LayerImpl;
class Pointf;
class Rect;

/** Each \a EditorMap consists out of one or more \a Layer,
    The \a Layer is an abstract base class from which the
    data holding layers derive. The basic functionality of a layer
    consists only of data holding and visualization. (FIXME: move
    visuals off into another class) */
class Layer
{
private:
public:
  Layer();
  Layer(std::shared_ptr<LayerImpl> i);
  ~Layer();

  MetaData get_metadata() const;

  /** Attaches a piece of MetaData to the layer, metadata is some user
      supplied piece of data that is associated with a layer (ie. the
      name of the layer or similar properties which aren't handled by
      the layer itself) */
  void set_metadata(MetaData data_);

  void draw(GraphicContext& gc);
  bool has_bounding_rect() const;
  Rect get_bounding_rect();

  /** Moves the layer to the given position */
  void set_pos(const Pointf& pos);

  /** Returns the current position of the layer */
  Pointf get_pos() const;
  bool is_null() const;

public:
  std::shared_ptr<LayerImpl> impl;
};

#endif

/* EOF */
