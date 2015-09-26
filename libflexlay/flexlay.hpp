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

#ifndef HEADER_FLEXLAY_FLEXLAY_HPP
#define HEADER_FLEXLAY_FLEXLAY_HPP

#include <boost/signals2.hpp>
#include <memory>

#include <QApplication>

class GUIManager;

/*! \mainpage Flexlay - A Flexible Layered 2D Editor

  \section intro Introduction

  Flexlay is a rather flexible layered editor mainly meant for editing
  game data such as levels, tilemaps, enemy placement and such. It
  provides a basic framework which makes it easy to add new
  functionality, special dialog windows and such needed to customize it
  for a specific game. Flexlay itself is actually a Python module and
  not an editor in itself, however due to reasonably simple Python
  scripts one can already have a fully working editor.

  \section structure Structure

  Flexlay provides the following basic classes on which everything else
  is build:

  Command: each operation on data is encapsuled in a Command object
  which provides undo/redo capability, together with a way to easily
  record macros and write scripts with it.

  EditorMapLayer: a map layer is the class that holds the data, special
  layers such as object or tilemap layers derive from this class to
  provide the capabilites needed to use them

  Tool: A tool manages and dispatches mouse input to Commands, thus
  giving the user an interactive way to manipulate map data.

  GUI: Flexlay provides a simple GUI framework that can be used from
  Python to create dialogboxes, add buttons to the main window and such.

  \section games Games

  Currently Flexlay supports the following games with different levels
  of completeness:

  netPanzer: fully working load/save and map editing capabilites

  SuperTux: fully working load/save and map editing capabilites, however
  a bit limited when it comes to object properties

  Windstille: fully working load/save support, however due to the game
  itself not being ready this is not so usefull

  Pingus: just very basic load support

*/

/** Flexlay holds the DisplayWindow and manages the graphic mode and
    screen resolution that should be. Its the top most class that
    needs to be inited before the rest becomes useable. FIXME: Make
    Flexlay 'batchable' so that it can run without a GUI */
class Flexlay
{
private:
  std::unique_ptr<QApplication> m_app;
  std::unique_ptr<GUIManager> m_gui_manager;

  int  m_screen_width;
  int  m_screen_height;

public:
  Flexlay();

  void set_datadir(const std::string& datadir_);

  GUIManager* create_gui_manager(const std::string& title);
};

#endif

/* EOF */
