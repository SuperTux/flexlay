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

#include "generated_brush.hpp"

class GeneratedBrushImpl : public BrushImpl
{
public:
  BrushShape shape;
  float  radius;
  int    spikes;
  float  hardness;
  float  aspect_ratio;
  float  angle;

  /** When set surface is out of date and needs updating */
  bool dirty;

  Sprite sprite;

  virtual ~GeneratedBrushImpl() {}
  Sprite get_sprite();
  void update();
  BrushImpl* clone() const;
};

GeneratedBrush::GeneratedBrush(const Brush& brush) :
  impl(std::dynamic_pointer_cast<GeneratedBrushImpl>(brush.impl))
{
}

GeneratedBrush::GeneratedBrush(BrushShape shape,
                               float  radius,
                               int    spikes,        /* 2 - 20     */
                               float  hardness,      /* 0.0 - 1.0  */
                               float  aspect_ratio,  /* y/x        */
                               float  angle) :
  impl(new GeneratedBrushImpl())
{
  impl->shape        = shape;
  impl->radius       = radius;
  impl->spikes       = spikes;
  impl->hardness     = hardness;
  impl->aspect_ratio = aspect_ratio;
  impl->angle        = angle;
  impl->dirty        = true;
}

void
GeneratedBrushImpl::update()
{
  if (dirty)
  {
#ifdef GRUMBEL
    CL_SpriteDescription desc;
    desc.add_frame(generate_brushmask(shape,
                                      radius,
                                      spikes,
                                      hardness,
                                      aspect_ratio,
                                      angle).to_cl());
    sprite = Sprite(desc);
    sprite.set_alignment(origin_center);
    dirty = false;
#endif
  }
}

void
GeneratedBrush::set_shape(BrushShape shape)
{
  impl->shape = shape;
  impl->dirty = true;
}

BrushShape
GeneratedBrush::get_shape()
{
  return impl->shape;
}

void
GeneratedBrush::set_radius(float radius)
{
  impl->radius = radius;
  impl->dirty = true;
}

float
GeneratedBrush::get_radius()
{
  return impl->radius;
}

void
GeneratedBrush::set_spikes(int spikes)
{
  impl->spikes = spikes;
  impl->dirty = true;
}

int
GeneratedBrush::get_spikes()
{
  return impl->spikes;
}

void
GeneratedBrush::set_hardness(float hardness)
{
  impl->hardness = hardness;
  impl->dirty = true;
}

float
GeneratedBrush::get_hardness()
{
  return impl->hardness;
}

void
GeneratedBrush::set_aspect_ratio(float aspect)
{
  impl->aspect_ratio = aspect;
  impl->dirty = true;
}

float
GeneratedBrush::get_aspect_ratio()
{
  return impl->aspect_ratio;
}

void
GeneratedBrush::set_angle(float angle)
{
  impl->angle = angle;
  impl->dirty = true;
}

float
GeneratedBrush::get_angle()
{
  return impl->angle;
}

Sprite
GeneratedBrushImpl::get_sprite()
{
  update();
  return sprite;
}

BrushImpl*
GeneratedBrushImpl::clone() const
{
  // FIXME: Make this Copy-On-Write cloning, else it might get a
  // little bit expensive with all the Sprite's per stroke
  GeneratedBrushImpl* c = new GeneratedBrushImpl();
  *c = *this;
  return c;
}

Brush
GeneratedBrush::to_brush()
{
  return Brush(impl);
}

/* EOF */
