//  $Id$
// 
//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#ifndef HEADER_LAYER_HXX
#define HEADER_LAYER_HXX

#include <ClanLib/Core/Math/rect.h>
#include "shared_ptr.hxx"

class EditorMapComponent;
class LayerImpl;

/** Each \a EditorMap consists out of one or more \a EditorMapLayer,
    The \a EditorMapLayer is an abstract base class from which the
    data holding layers derive. The basic functionality of a layer
    consists only of data holding and visualization. (FIXME: move
    visuals off into another class) */
class Layer
{
private:
public:
  Layer();
  Layer(SharedPtr<LayerImpl> i);
  ~Layer();

  void draw(EditorMapComponent* parent);
  bool has_bounding_rect() const;
  CL_Rect get_bounding_rect();

private:
  SharedPtr<LayerImpl> impl;
};

#endif

/* EOF */
