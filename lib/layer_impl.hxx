//  $Id$
// 
//  Flexlay - A Generic 2D Game Editor
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

#ifndef HEADER_LAYER_IMPL_HXX
#define HEADER_LAYER_IMPL_HXX

#include <ClanLib/Core/Math/rect.h>
#include <ClanLib/Display/graphic_context.h>
#include "meta_data.hxx"

class EditorMapComponent;

class LayerImpl
{
public:
  /** MetaData attached to the layer, MetaData can be any data
      supplied by the user, but most commonly it is used to associate
      the given layer with a scripting language object (PyObj, SCM,
      etc.), so that the user can attach additional data to a layer
      from the scripting side. */
  MetaData  data;
  
  /** The position of the layer */
  CL_Pointf pos;

  LayerImpl() {}
  virtual ~LayerImpl() {}

  virtual void draw(EditorMapComponent* parent, CL_GraphicContext* gc) =0;
  virtual bool has_bounding_rect() const =0;

  // FIXME: Should use CL_Rectf
  virtual CL_Rect get_bounding_rect() { return CL_Rect(); }
};

#endif

/* EOF */
