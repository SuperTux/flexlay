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

require "basic_gui.rb"
require "basic_level.rb"

## Init Flexlay itself
flexlay = Flexlay.new()
flexlay.init()

## Initialize Tools
class Controller
  attr_reader :tilemap_paint_tool, :tilemap_select_tool, :zoom_tool, :objmap_select_tool, :recent_files

  def initialize()
    @tilemap_paint_tool  = TileMapPaintTool.new()
    @tilemap_select_tool = TileMapSelectTool.new()
    @zoom_tool           = ZoomTool.new()
    @objmap_select_tool  = ObjMapSelectTool.new()
    @recent_files        = []
  end
    
  def set_tilemap_paint_tool()
    $gui.workspace.set_tool(@tilemap_paint_tool.to_tool())
    $gui.set_tilemap_paint_tool()
  end

  def set_tilemap_select_tool()
    $gui.workspace.set_tool(@tilemap_select_tool.to_tool())
    $gui.set_tilemap_select_tool()
  end

  def set_zoom_tool()
    $gui.workspace.set_tool(@zoom_tool.to_tool())
    $gui.set_zoom_tool()
  end

  def set_objmap_select_tool()
    $gui.workspace.set_tool(@objmap_select_tool.to_tool())
    $gui.set_objmap_select_tool()
  end  
end

$controller = Controller.new()

$mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

             
$resources = CL_ResourceManager.new("../data/flexlay.xml")

$tileset = Tileset.new(32)
$tileset.add_tile(1, Tile.new(make_pixelbuffer("../data/images/icons16/stock_paste-16.png")))
$tileset.add_tile(2, Tile.new(make_pixelbuffer("../data/images/icons24/stock_jump-to.png")))
$tileset.add_tile(3, Tile.new(make_pixelbuffer_from_resource("tile1", $resources),
                              make_sprite_from_resource("tile1", $resources)))
$tileset.add_tile(4, Tile.new(make_pixelbuffer_from_resource("tile2", $resources),
                              make_sprite_from_resource("tile2", $resources)))

## Create some basic GUI
$gui = GUI.new()

$gui.workspace.set_tool($controller.tilemap_paint_tool.to_tool());

startlevel = Level.new(100, 50)
startlevel.activate($workspace)

$gui.run()

flexlay.deinit()

# EOF #
