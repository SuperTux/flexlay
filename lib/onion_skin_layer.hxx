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

#ifndef HEADER_ONION_SKIN_LAYER_HXX
#define HEADER_ONION_SKIN_LAYER_HXX

#include "editor_map.hxx"

class OnionSkinLayerImpl;

/** The OnionSkinLayer is used to render one or multiple EditorMap
    renderings in a transparent fashion onto another EditorMap. This
    is usefull for animation programms and the like where one might
    need to see the previous or next frames together with the current
    frame. Might also be usefull for games which have shadow worlds,
    which reassamble the normal world in a darker fashion. */
class OnionSkinLayer
{
public:
  /** FIXME: Should probally be CL_Rect instead of just
      width/height */
  OnionSkinLayer(int width, int height);
  OnionSkinLayer(Layer layer);
  
  /** Adds an EditorMap to the OnionSkin */
  void add_map(EditorMap editor_map, const CL_Color& color);

  void clear();

  /** Refreshes the content of the OnionSkin */
  void update();
  
  bool is_null() const { return !impl.get(); }
  Layer to_layer();
private:
  SharedPtr<OnionSkinLayerImpl> impl;  
};

#endif

/* EOF */
