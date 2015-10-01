# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require "flexlay_wrap"
include Flexlay_wrap

module GameObjects
  class GameObject
    attr_reader :data

    def data=(data)
      @data = data
      connect_v1_ObjMapObject(@data.to_object.sig_move(), method(:on_move))
      on_move(@data)
    end

    def get_sprite()
      self.class.get_sprite()
    end

    def on_move(data)
      pos = @data.to_object.get_pos()
      pos.x = (((pos.x+16)/32).to_i)*32
      pos.y = (((pos.y+16)/32).to_i)*32
      @data.to_object.set_pos(pos)
    end

    def draw_to_tilemap(tilemap)
      # Draws the GameObject to the given TilemapLayer
    end
  end

  class Outpost < GameObject
    attr_accessor :name

    def initialize()
      @name = "Foobar"
    end

    def x()
      return (@data.to_object.get_pos.x()/32).to_i
    end

    def y()
      return (@data.to_object.get_pos.y()/32).to_i
    end

    def Outpost.get_sprite()
      return make_sprite("netpanzersprites/outpost.png")
    end

    def Outpost.create(objmap, name, x, y)
      obj = Outpost.new()
      obj.name = name
      sprite_obj = ObjMapSpriteObject.new(get_sprite(), Pointf.new(x*32, y*32).to_cl(),
                                          make_metadata(obj))
      obj.data = sprite_obj
      objmap.add_object(sprite_obj.to_object)

      return obj
    end

    def draw_to_tilemap(tilemap)
      start  = 9541
      width  = 32
      height = 18

      brush = TileBrush.new(width, height)
      brush.set_data(Range.new(start, start + (width*height)-1).to_a)
      tilemap.draw_tile(brush, Point.new(x()-18, y()-6).to_cl())
    end
  end

  class SpawnPoint < GameObject
    def x()
      return (@data.to_object.get_pos.x()/32).to_i
    end

    def y()
      return (@data.to_object.get_pos.y()/32).to_i
    end

    def SpawnPoint.create(objmap, x, y)
      obj = SpawnPoint.new()
      sprite_obj = ObjMapSpriteObject.new(get_sprite(), Pointf.new(x*32, y*32),
                                          make_metadata(obj))
      obj.data = sprite_obj
      objmap.add_object(sprite_obj.to_object)

      return obj
    end

    def SpawnPoint.get_sprite()
      return make_sprite("netpanzersprites/spawnpoint.png")
    end
  end

  # TileObject is used to hold tilebrushes
  class TileObject < GameObject
    def initialize(brushindex)
      @brushindex = brushindex
    end

    def x()
      return (@data.to_object.get_pos.x()/32).to_i
    end

    def y()
      return (@data.to_object.get_pos.y()/32).to_i
    end

    def draw_to_tilemap(tilemap)
      (start, width, height, @name) = $brushes[@brushindex]

      # FIXME: Make this undoable
      brush = TileBrush.new(width, height)
      brush.set_data(Range.new(start, start + (width*height)-1).to_a)

      tilemap.draw_tile(brush, Point.new(x(), y()).to_cl())
    end

    def get_sprite()
      sprite = NetPanzerData::instance().get_tilegroup_sprite($brushes[@brushindex][0])
      return sprite
    end

    def TileObject.create(objmap, brushindex, x, y)
      obj = TileObject.new(brushindex)
      sprite_obj = ObjMapSpriteObject.new(NetPanzerData::instance().get_tilegroup_sprite($brushes[brushindex][0]),
                                          Pointf.new(x*32, y*32),
                                          make_metadata(obj))
      obj.data = sprite_obj
      objmap.add_object(sprite_obj.to_object)

      return obj
    end
  end
end

# EOF #
