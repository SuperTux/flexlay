/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software: you can redistribute it and/or modify
**  it under the terms of the GNU General Public License as published by
**  the Free Software Foundation, either version 3 of the License, or
**  (at your option) any later version.
**  
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
**  
**  You should have received a copy of the GNU General Public License
**  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef HEADER_DRAWING_CONTEXT_HPP
#define HEADER_DRAWING_CONTEXT_HPP

#include "SDL.h"
#include "math/rect.hpp"
#include "stroke.hpp"

class Color;
class GrayscaleBuffer;
class DrawingParameter;
class StrokeBuffer;

class DrawingContext
{
private:
  SDL_Surface*     drawable;
  StrokeBuffer*    stroke_buffer;
 
public:
  DrawingContext(int w, int h);
  ~DrawingContext();

  void draw_stroke(const Stroke& stroke, DrawingParameter* param); 

  void clear();

  void draw(SDL_Surface* target, const Rect& region, int x_of, int y_of);

  /** Blits \a source onto \a drawable at position \a pos */
  void blit(SDL_Surface* source, const Point& pos);

  int get_width()  const { return drawable->w; }
  int get_height() const { return drawable->h; }
  bool get_color(int x, int y, Color& color);
  SDL_Surface* get_surface() { return drawable; }

  /** Creates a surface of the given region */
  SDL_Surface* get_surface(const Rect& rect);

  void save_png(const std::string& filename);
};

#endif

/* EOF */
