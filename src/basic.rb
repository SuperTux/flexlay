##  $Id$
## 
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2004 Ingo Ruhnke <grumbel@gmx.de>
##
##  This program is free software; you can redistribute it and/or
##  modify it under the terms of the GNU General Public License
##  as published by the Free Software Foundation; either version 2
##  of the License, or (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
## 
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

## A basic tile editor that should act as example for other games, use
## it to fork your own code.

$datadir = ""

## First we try to read a config file to set some variables
$config_file = File.expand_path("~/.flexlay/basic.rb")

if File.exist?($config_file) then
  require $config_file
end

BACKGROUND_LAYER  = 1
INTERACTIVE_LAYER = 2
FOREGROUND_LAYER  = 3

## Load Flexlay library
require "flexlay_wrap"
include Flexlay_wrap

require "flexlay.rb"

## Init Flexlay itself
flexlay = Flexlay.new()
flexlay.init()

## Init the GUI manager
$editor = Editor.new()
$gui = $editor.get_gui_manager()

## Create some basic GUI
myrect      = CL_Rect.new(CL_Point.new(0, 56), CL_Size.new(665, 488+56))
$editor_map = EditorMapComponent.new(myrect, $gui.get_component())
$workspace  = Workspace.new(myrect.get_width(), myrect.get_height())
$editor_map.set_workspace($workspace)

## Initialize Tools
$tilemap_paint_tool  = TileMapPaintTool.new()
$tilemap_select_tool = TileMapSelectTool.new()
$zoom_tool           = ZoomTool.new()
$objmap_select_tool  = ObjMapSelectTool.new()

$workspace.set_tool($tilemap_paint_tool.to_tool());

$mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

$minimap = Minimap.new($editor_map, CL_Rect.new(CL_Point.new(3 + myrect.left, 
                                                             488+3-14  + myrect.top), 
                                                CL_Size.new(794-134-16, 50)), 
                       $gui.get_component())

$tileset = Tileset.new(32)

require "basic_level.rb"
require "basic_gui.rb"

$gui.run()

flexlay.deinit()

# EOF #
