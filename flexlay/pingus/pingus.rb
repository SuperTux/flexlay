#!/usr/bin/ruby
##  $Id$
##
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2005 Ingo Ruhnke <grumbel@gmx.de>
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

$datadir = "/home/ingo/projects/pingus/svn/trunk/data/"

require "flexlay_wrap"
include Flexlay_wrap

require "flexlay.rb"

flexlay = Flexlay.new()
flexlay.init()

$objmap_select_tool  = ObjMapSelectTool.new()
$zoom_tool           = ZoomTool.new()

require "worldmap.rb"
require "level.rb"
require "worldobjs.rb"
require "gui.rb"

$gui = GUI.new()

$resources = CL_ResourceManager.new()
$resources.add_resources(CL_ResourceManager.new($datadir + "data/groundpieces-ground.xml"))
$resources.add_resources(CL_ResourceManager.new($datadir + "data/groundpieces-solid.xml"))
$resources.add_resources(CL_ResourceManager.new($datadir + "data/groundpieces-transparent.xml"))
$resources.add_resources(CL_ResourceManager.new($datadir + "data/groundpieces-remove.xml"))

$resources.add_resources(CL_ResourceManager.new($datadir + "data/core.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/entrances.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/exits.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/fonts.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/game.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/hotspots.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/liquids.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/pingus.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/story.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/textures.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/traps.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/worldmaps.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/worldobjs.xml"));
$resources.add_resources(CL_ResourceManager.new($datadir + "data/alias.xml"));

if false then
  worldmap = WorldMap.new()
  worldmap.activate($gui.workspace)
else
  level = Level.new($datadir + 'levels/tutorial/snow22-grumbel.pingus')
  level.activate($gui.workspace)
  level.save("/tmp/levelout.xml")
end

# puts $resources.get_all_sections()

$resources.get_resources_of_type("sprite", "groundpieces/ground/").each { |name|
  begin
    $gui.objectselector.add_brush(ObjectBrush.new(CL_Sprite.new(name, $resources),
                                                  make_metadata(["groundpiece", name])))
  rescue CL_Error => err
    puts err.message
  end
}

$gui.workspace.set_tool($objmap_select_tool.to_tool())

$gui.run()

# flexlay.deinit()

# EOF #
