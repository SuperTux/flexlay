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

require "gameobjects.rb"

class Level
  attr_accessor :filename, :data, :editormap, :objects

  def initialize(*params)
    if params.length == 2 then
      (width, height) = params
      @data = NetPanzerFileStruct.new($tileset, width, height)

    elsif params.length == 1 then
      (@filename,) = params
      @data = NetPanzerFileStruct.new($tileset, @filename)
    end      

    @objects   = ObjectLayer.new()
    @editormap = EditorMap.new()
    @editormap.add_layer(@data.get_tilemap().to_layer())
    @editormap.add_layer(@objects.to_layer())

    # FIXME: Data might not get freed since its 'recursively' refcounted
    @editormap.set_metadata(make_metadata(self))
  end

  def save_optfile(filename)
    outposts = @objects.get_objects().
      find_all {|obj| obj.get_data().is_a? GameObjects::Outpost }.
      map {|obj| obj.get_data() }    

    f = open(filename, "w")
    f.write("ObjectiveCount: %d\n\n" % outposts.length)
    outposts.each {|obj|
      f.write("Name: %s\n" % obj.name)
      f.write("Location: %d %d\n\n" % [obj.x, obj.y])
    }
    f.close()
  end

  def save_spnfile(filename)
    spawnpoints = @objects.get_objects().
      find_all {|obj| obj.get_data().is_a? GameObjects::SpawnPoint }.
      map {|obj| obj.get_data() }

    f = open(filename, "w")

    f.write("SpawnCount: %d\n\n" % spawnpoints.length)
    spawnpoints.each {|obj|
      f.print("Location: %d %d\n" % [obj.x, obj.y])
    }
    f.close()
  end

  def save(filename)
    if filename[-4..-1] == ".npm"
      data.save(filename)
      save_optfile(filename[0..-5] + ".opt")
      save_optfile(filename[0..-5] + ".spn")
    else
      raise "Fileextension not valid, must be .npm!"
    end
  end

  def activate(workspace)
    $workspace.set_map(@editormap)
    TilemapLayer.set_current(@data.get_tilemap())
    ObjectLayer.set_current(@objects)
  end
end

# EOF #
