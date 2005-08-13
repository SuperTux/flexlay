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

$datadir = "/home/ingo/projects/windstille/trunk/data/"

## First we try to read a config file to set some variables
$config_file = File.expand_path("~/.flexlay/windstille.rb")

if File.exist?($config_file) then
  require $config_file
end

## Load Flexlay library
require "flexlay_wrap"
include Flexlay_wrap

require "flexlay.rb"

require "controller.rb"
require "sexpr.rb"
require "gui.rb"
require "sector.rb"
require "tileset.rb"

## Init Flexlay itself
flexlay = Flexlay.new()
flexlay.init(1024, 768)

## Initialize Tools

$controller = Controller.new()

$mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

             
$resources = CL_ResourceManager.new("../data/flexlay.xml")

$tileset = Tileset.new(32)
$tileset.load($datadir + "tiles.scm")

## Create some basic GUI
$gui = GUI.new()

$gui.workspace.set_tool($controller.tilemap_paint_tool.to_tool());

$startlevel = Sector.new(100, 30)
$startlevel.activate($workspace)

$gui.run()

# flexlay.deinit()

# EOF #
