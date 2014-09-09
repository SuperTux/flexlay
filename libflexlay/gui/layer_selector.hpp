// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
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

#ifndef HEADER_LAYER_SELECTOR_HPP
#define HEADER_LAYER_SELECTOR_HPP

class QListView;
class QScrollArea;
class QToolBar;
class QTreeView;
class QVBoxLayout;
class QWidget;

class LayerSelector final
{
public:
#ifndef SWIG
  LayerSelector();

  QWidget* get_widget() const;
#endif

private:
  QWidget* m_vbox;
  QTreeView* m_tree_view;
  QVBoxLayout* m_layout;

  QScrollArea* m_scroll_area;
  QToolBar* m_toolbar;

private:
  LayerSelector(const LayerSelector&) = delete;
  LayerSelector& operator=(const LayerSelector&) = delete;
};

#endif

/* EOF */
