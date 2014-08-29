//  Flexlay - A Generic 2D Game Editoryy
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

#ifndef HEADER_FLEXLAY_STROKE_DRAWER_IMPL_HPP
#define HEADER_FLEXLAY_STROKE_DRAWER_IMPL_HPP

class CL_GraphicContext;
class Stroke;

/** Abstract class which handles the drawing of a Stroke
 */
class StrokeDrawerImpl
{
private:
public:
  virtual ~StrokeDrawerImpl() {}

  virtual void draw(const Stroke& stroke, CL_GraphicContext* gc) =0;
  virtual StrokeDrawerImpl* clone() const =0;
};

#endif

/* EOF */
