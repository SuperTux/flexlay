##  $Id$
## 
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
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

# Handle command line arguments
if ARGV != []
  puts "Usage: ./netpanzer-editor"
  exit
end

require "flexlay_wrap"
include Flexlay_wrap

require "flexlay.rb"
require "netpanzer_wrap"
include Netpanzer_wrap

require "netpanzerbrushes.rb"
require "level.rb"
require "gameobjects.rb"
require "gui.rb"

$screen  = CL_Size.new(1024, 768)

flexlay = Flexlay.new()
flexlay.init($screen.width, $screen.height)

class Config
  attr_accessor :datadir, :recent_files

  def initialize()
    @datadir      = "./"
    @recent_files = []
  end
end

$config = Config.new()

$datadir = "/home/ingo/games/netpanzer-0.1.5"

$brushes.each_with_index{|(start, width, height, name), index|
  NetPanzerData::instance().register_tilegroup(start, width, height)
}

NetPanzerData::instance().load_data($datadir)

$tileset = NetPanzerData::instance().get_tileset()

$resources = CL_ResourceManager.new("netpanzersprites.xml")

# Tools
$tilemap_paint_tool  = TileMapPaintTool.new()
$tilemap_select_tool = TileMapSelectTool.new()
$zoom_tool           = ZoomTool.new()
$objmap_select_tool  = ObjMapSelectTool.new()


def generate_sprites()
  $brushes.each_with_index{|(start, width, height, name), index|
    puts "#{index}"
    buffer = make_pixelbuffer(width * 32, height * 32)

    (0..(height-1)).each {|y|
      (0..(width-1)).each {|x|
        tile = $tileset.create(start + width * y + x)
        blit(buffer, tile.get_pixelbuffer(), x * 32, y * 32)
      }
    }
    
    CL_ProviderFactory.save(buffer, "sprites/#{index}.png")
  }
end

# generate_sprites()

$gui = GUI.new()

$gui.set_tilemap_paint_tool()

$gui.gui_level_new()

# generate_sprites()
$gui.run()

# flexlay.deinit()
# print "deinit done"

# EOF #
