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
      when :tile
        data   = i[1..-1]
        id     = get_value_from_tree(['id', '_'], data, -1)
        image  = get_value_from_tree(['editor-images', '_'], data, false)
        hidden = get_value_from_tree(['hidden', '_'], data, false)

        # puts "Loading tile: #{id} => #{image}"

        if not(image)
          image = get_value_from_tree(['images', '_'], data, "notile.png")
        end
        
        if image.is_a?(String) then
          pixelbuffer = make_pixelbuffer($datadir + 'images/' + image)
        elsif image.is_a?(Array) then
          if image[0] == "region" then
            pixelbuffer = scale_pixelbuffer(make_region_pixelbuffer($datadir + 'images/' + image[1],
                                                                    image[2], image[3], image[4], image[5]))
          end
        end
        
        if not hidden then
          if id == 0 then
            add_tile(id, nil)
          else
            add_tile(id, Tile.new(pixelbuffer))
          end
        end

      when :tilegroup
        data  = i[1..-1]
        name  = get_value_from_tree(['name', '_'], data, "Unnamed")
        tiles = get_value_from_tree(['tiles'], data, [])
        
        if not $tileset.tilegroups then
          $tileset.tilegroups = []
        end
        $tileset.tilegroups.push(TileGroup.new(name, tiles))
        
      when :tiles
        data   = i[1..-1]
        colmap  = get_value_from_tree(['colmap'], data, [])
        ids     = get_value_from_tree(['ids'], data, [])
        image = get_value_from_tree(['image', '_'], data, "")
        
        width  = get_value_from_tree(['width', '_'], data, 0)
        height = get_value_from_tree(['height', '_'], data, 0)

        puts "Loading: #{width}x#{height} #{$datadir + image}"

        pixelbuffer = make_pixelbuffer($datadir + image);

        (0..height-1).each {|y|
          (0..width-1).each {|x|
            if (y*width + x < ids.length) then
              $tileset.add_tile(ids[y*width + x],
                                Tile.new(scale_pixelbuffer(make_region_pixelbuffer(pixelbuffer,
                                                                                   64*x, 64*y, 64, 64))))
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

