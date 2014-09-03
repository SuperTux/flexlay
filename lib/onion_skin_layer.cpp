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

#include "onion_skin_layer.hpp"

#include <iostream>
#include <vector>
#include <ClanLib/Core/System/error.h>
#include <ClanLib/Display/pixel_buffer.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/surface.h>
#include <ClanLib/Display/canvas.h>
#include <ClanLib/Display/graphic_context.h>

#include "gui/editor_map_component.hpp"
#include "layer_impl.hpp"

#define SCALE 4

class OnionSkinLayerImpl : public LayerImpl
{
public:
  CL_Surface  surface;
  CL_Canvas*  canvas;

  CL_Surface  surface2;
  CL_Canvas*  canvas2;

  std::vector<EditorMap> editormaps;
  std::vector<Color>  color;

  void draw(GraphicContext& gc)
  {
    // FIXME: We need to stop onion layer to draw onto itself
    surface.set_blend_func(blend_one, blend_one_minus_src_alpha);
    surface.set_scale(SCALE, SCALE);
    surface.draw(0, 0);
  }

  bool has_bounding_rect() const
  {
    return false;
  }
};

OnionSkinLayer::OnionSkinLayer(Layer layer) :
  impl(std::dynamic_pointer_cast<OnionSkinLayerImpl>(layer.impl))
{
}

OnionSkinLayer::OnionSkinLayer(int width, int height) :
  impl(new OnionSkinLayerImpl())
{
  impl->surface  = CL_Surface(CL_PixelBuffer(width/SCALE, height/SCALE, width*4/SCALE, CL_PixelFormat::rgba8888));
  impl->surface2 = CL_Surface(CL_PixelBuffer(width/SCALE, height/SCALE, width*4/SCALE, CL_PixelFormat::rgba8888));

  try
  {
    impl->canvas = new CL_Canvas(impl->surface);
    impl->canvas->get_gc()->clear(Color(0, 0, 0, 0).to_cl());
    impl->canvas->get_gc()->flush();
    impl->canvas->sync_surface();

    impl->canvas2 = new CL_Canvas(impl->surface2);
    impl->canvas2->get_gc()->clear(Color(0, 0, 0, 0).to_cl());
    impl->canvas2->get_gc()->flush();
    impl->canvas2->sync_surface();
  }
  catch(const CL_Error& err)
  {
    std::cout << "CL_Error: " << err.message << std::endl;
    throw err;
  }
}

void
OnionSkinLayer::clear()
{
  impl->canvas->get_gc()->clear(Color(0, 0, 0, 0).to_cl());
  impl->canvas->sync_surface();
}

void
OnionSkinLayer::add_map(EditorMap editor_map, const Color& color)
{
  impl->editormaps.push_back(editor_map);
  impl->color.push_back(color);
}

void
OnionSkinLayer::update()
{
#ifdef GRUMBEL
  impl->canvas->get_gc()->clear(Color(0, 0, 0, 0).to_cl());
  for (std::vector<EditorMap>::size_type i = 0; i < impl->editormaps.size(); ++i)
  {
    impl->canvas2->get_gc()->clear(Color(0, 0, 0, 0).to_cl());
    impl->canvas2->get_gc()->push_modelview();
    impl->canvas2->get_gc()->add_scale(1.0f/SCALE, 1.0f/SCALE);

    impl->editormaps[i].draw(EditorMapComponent::current()->get_gc_state(), impl->canvas2->get_gc());

    impl->canvas2->get_gc()->pop_modelview();

    impl->canvas2->sync_surface();

    impl->surface2.set_color(impl->color[i].to_cl());
    impl->surface2.draw(0, 0, impl->canvas->get_gc());
    impl->canvas->sync_surface();
  }
#endif
}

Layer
OnionSkinLayer::to_layer()
{
  return Layer(impl);
}

/* EOF */

