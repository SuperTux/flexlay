//  $Id$
// 
//  Flexlay - A Generic 2D Game Editor
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

#ifndef HEADER_FLEXLAY_HXX
#define HEADER_FLEXLAY_HXX

#include <ClanLib/Display/display_window.h>
#include <ClanLib/Core/Resources/resource_manager.h>

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
  CL_DisplayWindow* window;

public:
  int  screen_width;
  int  screen_height;
  bool fullscreen;
  bool allow_resize;
  bool use_opengl;

  CL_ResourceManager resources;

  static Flexlay* current() { return current_; }
public:
  static Flexlay* current_;

  Flexlay();

  void init(int width = 800, int height = 600, bool fullscreen = false);
  void deinit();
};

#endif

/* EOF */
