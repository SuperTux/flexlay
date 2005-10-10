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

class TileGroup
  attr_accessor :name, :tiles
  
  def initialize(name, tiles)
    @name  = name
    @tiles = tiles
  end 
end

class Tileset
  attr_accessor :tilegroups

  def load(filename)
    puts "Loading Tileset: #{filename}"
    tree = sexpr_read_from_file(filename)

    if tree == nil then
      puts "Error; Couldn't load: ", filename
      return 
    end

    tree = tree[1..-1]
    counter = 0
    
    tree.each do |i|
      case i[0]       
      when :tiles
        data   = i[1..-1]
        colmap = get_value_from_tree(['colmap'], data, [])
        ids    = get_value_from_tree(['ids'], data, [])
        image  = get_value_from_tree(['image', '_'], data, "")
        
        width  = get_value_from_tree(['width', '_'], data, 0)
        height = get_value_from_tree(['height', '_'], data, 0)

        puts "Loading: Size: #{width}x#{height} Filename: #{$datadir + image}"

        pixelbuffer = make_pixelbuffer($datadir + image);

        (0..height-1).each {|y|
          (0..width-1).each {|x|
            if (y*width + x < ids.length) then
              $tileset.add_tile(ids[y*width + x],
                                Tile.new(make_region_pixelbuffer(pixelbuffer,
                                                                 16*x, 16*y, 16, 16)))
            else
              puts "Id out of range: #{y*width + x} >= #{ids.length} for image #{$datadir + image}"
            end
          }
        }
      end
    end
    puts ""
  end
end

# EOF #
