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

#ifndef HEADER_FLEXLAY_EDITOR_MAP_HPP
#define HEADER_FLEXLAY_EDITOR_MAP_HPP

#include <boost/signals2.hpp>

#include "layer.hpp"
#include "command.hpp"
#include "color.hpp"

class GraphicContext;
class EditorMapImpl;
class Layer;
class MetaData;

/** Object which represents a level, quirled together with the GUI
    stuff */
class EditorMap
{
public:
  EditorMap(bool create = false);

  void draw(GraphicContext& gc);

  /** Draw stuff that is only relevant on the GUI (bounding rects and such) */
  void draw_gui(GraphicContext& gc);

  void add_layer(const Layer& layer, int pos = -1);

  bool is_modified() const;
  void set_unmodified();
  void modify();

  int get_serial() const;

  int get_layer_count() const;
  Layer get_layer(int i);

  void   set_metadata(const MetaData& obj);
  MetaData get_metadata() const;

  bool has_bounding_rect() const;
  Rect get_bounding_rect();

  /** Set the bounding rect for this map, if the given rect is
      Rect() the bounding rect will be calculated automatically
      from the content of the map */
  void set_bounding_rect(const Rect& rect);

  void set_background_color(const Color& color);

  /** Execute a command and place it on the undo stack, commands given
      to this function will be deleted by the Editor class, so they
      have to be new'ed */
  void execute(Command command);

  /** Move backward in the undo stack */
  void undo();

  /** Move forward in the undo stack */
  void redo();

  int undo_stack_size();

  int redo_stack_size();

  boost::signals2::signal<void ()>& sig_change();

  explicit operator bool() const { return static_cast<bool>(impl); }

private:
  std::shared_ptr<EditorMapImpl> impl;
};

#endif

/* EOF */
