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

#ifndef HEADER_FLEXLAY_EDITOR_MAP_COMPONENT_HPP
#define HEADER_FLEXLAY_EDITOR_MAP_COMPONENT_HPP

#include <boost/signals2.hpp>
#include <memory>

#include "workspace.hpp"

class EditorMapWidget;
class QGridLayout;
class QScrollBar;
class QWidget;

/** Object which represents a level, quirled together with the GUI
    stuff */
class EditorMapComponent
{
private:
  static EditorMapComponent* current_;
protected:
  virtual ~EditorMapComponent();
public:
  static EditorMapComponent* current() { return current_; }

  EditorMapComponent(QWidget* parent);

  Workspace get_workspace() const;

  void set_zoom(float z);
  void zoom_to(Rectf rect);
  void zoom_out(Point pos);
  void zoom_in (Point pos);

  void move_to(int x, int y);
  void move_to_x(float x);
  void move_to_y(float y);

  void capture_mouse();
  void release_mouse();

#ifdef GRUMBEL
  boost::signals2::signal<void (int, int)>& sig_on_key(const std::string& str);
#endif

  Pointf screen2world(const Point& pos);

  Rectf get_clip_rect() const;

  GraphicContextState& get_gc_state();

  EditorMapWidget* get_editormap_widget() const { return m_editormap_widget; }
  QWidget* get_widget() const;

private:
  void update_scrollbars();

private:
  QWidget* m_widget;
  QGridLayout* m_layout;
  QScrollBar* m_scroll_horz;
  QScrollBar* m_scroll_vert;

  EditorMapWidget* m_editormap_widget;
  Workspace m_workspace;
  GraphicContextState m_gc_state;
};

#endif

/* EOF */
