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

module GameObjects
  class GameObject
    attr_reader :data
    
    def data=(data)
      @data = data
      connect_v1_ObjMapObject(@data.to_object.sig_move(), method(:on_move))
    end

    def on_move(data)
      pos = @data.to_object.get_pos()
      pos.x = (((pos.x+16)/32).to_i)*32
      pos.y = (((pos.y+16)/32).to_i)*32
      @data.to_object.set_pos(pos)
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
      return make_sprite_from_resource("sprites/outpost", $resources)
    end
  end
  
  class SpawnPoint < GameObject
    def x()
      return (@data.to_object.get_pos.x()/32).to_i
    end

    def y()
      return (@data.to_object.get_pos.y()/32).to_i
    end

    def SpawnPoint.get_sprite()
      return make_sprite_from_resource("sprites/spawnpoint", $resources)
    end
  end
end

class Config
  attr_accessor :datadir, :recent_files

  def initialize()
    @datadir      = "./"
    @recent_files = []
  end
end

# EOF #
