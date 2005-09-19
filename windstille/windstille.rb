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

## First we try to read a config file to set some variables
## Load Flexlay library
require "flexlay_wrap"
include Flexlay_wrap

require "flexlay.rb"

require "controller.rb"
require "sexpr.rb"
require "sexpr_config_file.rb"
require "gui.rb"
require "sector.rb"
require "tileset.rb"


$config = SExprConfigFile.new("windstille-editor") {
  register("datadir",       nil)
  register("screen-width",  800)
  register("screen-height", 600)
  register("fullscreen",    false)
  register("recent-files",  [])
}

$datadir = $config.get("datadir")

$screen_width  = $config.get("screen-width")
$screen_height = $config.get("screen-height")

## Init Flexlay itself
$flexlay = Flexlay.new()
$flexlay.init($screen_width, $screen_height, false, true)
$gui_manager = GUIManager.new()

if $datadir == nil or not File.exist?($datadir) then
  dialog = GenericDialog.new("Windstille Data Directory", $gui_manager.get_component())
  dialog.add_label("You need to specify the datadir of Windstille is located")
  dialog.add_string("Datadir:", $datadir || "")
  
  dialog.set_block { |datadir|
    $datadir = datadir 
    $gui_manager.quit()
  }
  $gui_manager.run()
end

## Initialize Tools
$controller = Controller.new()

$resources = CL_ResourceManager.new("../data/flexlay.xml")

$tileset = Tileset.new(32)
$tileset.load($datadir + "tiles.scm")

## Create some basic GUI
$gui = GUI.new()

$gui.workspace.set_tool($controller.tilemap_paint_tool.to_tool());

$startlevel = Sector.new(100, 30)
$startlevel.activate($workspace)

$gui.run()

$config.set("datadir", $datadir)
$config.write()

# $flexlay.deinit()

# EOF #
