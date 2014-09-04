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

#ifndef HEADER_FLEXLAY_ONION_SKIN_LAYER_HPP
#define HEADER_FLEXLAY_ONION_SKIN_LAYER_HPP

#include "editor_map.hpp"

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
  /** FIXME: Should probally be Rect instead of just
      width/height */
  OnionSkinLayer(int width, int height);
  OnionSkinLayer(Layer layer);

  /** Adds an EditorMap to the OnionSkin */
  void add_map(EditorMap editor_map, const Color& color);

  void clear();

  /** Refreshes the content of the OnionSkin */
  void update();

  bool is_null() const { return !impl.get(); }
  Layer to_layer();
private:
  std::shared_ptr<OnionSkinLayerImpl> impl;
};

#endif

/* EOF */
