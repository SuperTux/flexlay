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

#include <iostream>
#include <ClanLib/Core/System/error.h>
#include <ClanLib/Display/pixel_buffer.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/surface.h>
#include <ClanLib/Display/canvas.h>
#include "editor_map_component.hxx"
#include "layer_impl.hxx"
#include "onion_skin_layer.hxx"

class OnionSkinLayerImpl : public LayerImpl
{
public:
  CL_Surface  surface;
  CL_Canvas*  canvas;

  CL_Surface  surface2;
  CL_Canvas*  canvas2;

  void draw(EditorMapComponent* parent, CL_GraphicContext* gc) 
  {
    // FIXME: We need to stop onion layer to draw onto itself
    surface.draw(0, 0);
  }

  bool has_bounding_rect() const
  {
    return false;
  }
};

OnionSkinLayer::OnionSkinLayer(int width, int height)
  : impl(new OnionSkinLayerImpl())
{
  impl->surface  = CL_Surface(CL_PixelBuffer(width, height, width*4, CL_PixelFormat::rgba8888));
  impl->surface2 = CL_Surface(CL_PixelBuffer(width, height, width*4, CL_PixelFormat::rgba8888));

  try
    {
      impl->canvas = new CL_Canvas(impl->surface);
      impl->canvas->get_gc()->clear(CL_Color(0, 0, 0, 0));
      impl->canvas->get_gc()->flush();
      impl->canvas->sync_surface();

      impl->canvas2 = new CL_Canvas(impl->surface2);
      impl->canvas2->get_gc()->clear(CL_Color(0, 0, 0, 0));
      impl->canvas2->get_gc()->flush();
      impl->canvas2->sync_surface();
    }
  catch(CL_Error& err) 
    {
      std::cout << "CL_Error: " << err.message << std::endl;
      throw err;
    }
}

void
OnionSkinLayer::clear()
{
  impl->canvas->get_gc()->clear();
  impl->canvas->sync_surface();
}

void
OnionSkinLayer::add_map(EditorMap editor_map, float transparency)
{
  // FIXME: Parameter are a bit unclear here
  impl->canvas2->get_gc()->clear();
  editor_map.draw(EditorMapComponent::current(), impl->canvas2->get_gc());
  impl->canvas2->sync_surface();
  impl->surface2.set_alpha(transparency);
  impl->surface2.draw(0, 0, impl->canvas->get_gc());
  impl->canvas->sync_surface();
}

Layer
OnionSkinLayer::to_layer()
{
  return Layer(impl);
}

/* EOF */

