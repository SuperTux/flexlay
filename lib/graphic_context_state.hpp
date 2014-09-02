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

#ifndef HEADER_FLEXLAY_GRAPHIC_CONTEXT_STATE_HPP
#define HEADER_FLEXLAY_GRAPHIC_CONTEXT_STATE_HPP

#include <ClanLib/Core/Math/rect.h>
#include <memory>

class CL_GraphicContext;

class GraphicContextStateImpl;

/** Helper class for capturing the state of a GraphicContext, with
    additional convenience functions to make handling GraphicContexts
    easier */
class GraphicContextState
{
public:
  GraphicContextState();
  GraphicContextState(int w, int h);

  void set_size(int w, int h);

  void push(CL_GraphicContext* gc = 0) const;
  void pop (CL_GraphicContext* gc = 0) const;

  /** Return a rectangle in world coordinates that represents the area
      visible on the screen */
  CL_Rectf get_clip_rect() const;

  int get_width()  const;
  int get_height() const;

  /** Set the current rotation angel */
  void  set_rotation(float angle);

  /** Return the current rotation angel */
  float get_rotation() const;

  /** Move the center of the visible area to pos */
  void      set_pos(const CL_Pointf& pos);
  CL_Pointf get_pos() const;

  /** Set zoom to z, while ensuring that the screen position \a pos
      (normaly the position of the mouse pointer) stays in the same
      position even after zoomed in/out */
  void  set_zoom(CL_Pointf pos, float z);
  void  set_zoom(float z);
  float get_zoom() const;

  void zoom_to (const CL_Rectf& rect);

  CL_Pointf screen2world(const CL_Point& pos);

private:
  std::shared_ptr<GraphicContextStateImpl> impl;
};

#endif

/* EOF */
