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

#include "brush_impl.hxx"
#include "generated_brush.hxx"

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

  CL_Surface surface;

  virtual ~GeneratedBrushImpl() {}
  CL_Surface get_surface();
  void update();
  BrushImpl* clone() const;
};

GeneratedBrush::GeneratedBrush(const Brush& brush)
: impl(dynamic_cast<GeneratedBrushImpl*>(brush.impl.get())) // FIXME: WANT WORK WITH REAL SMARTPTR!!!
{
}

GeneratedBrush::GeneratedBrush(BrushShape shape,
                               float  radius,
                               int    spikes,        /* 2 - 20     */
                               float  hardness,      /* 0.0 - 1.0  */
                               float  aspect_ratio,  /* y/x        */
                               float  angle)
  : impl(new GeneratedBrushImpl())
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
      surface = CL_Surface(new CL_PixelBuffer(generate_brushmask(shape,
                                                                 radius, 
                                                                 spikes,
                                                                 hardness, 
                                                                 aspect_ratio, 
                                                                 angle)), 
                           true);
      surface.set_alignment(origin_center);
      dirty = false;
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

CL_Surface
GeneratedBrushImpl::get_surface() 
{
  update();
  return surface;
}

BrushImpl*
GeneratedBrushImpl::clone() const
{
  // FIXME: Make this Copy-On-Write cloning, else it might get a
  // little bit expensive with all the CL_Surface's per stroke
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
