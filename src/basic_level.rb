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

class Level
  ## Level Properties
  attr_reader :layers, :editormap
  
  attr_reader :name
  attr_writer :name

  def initialize(*params)
    if params.length() == 2 then
      # New Level
      (width, height) = params

      @width  = width
      @height = height

      @current_layer = 0

      @layers = []
      @layers += [TilemapLayer.new($tileset, @width, @height)]
      @layers += [TilemapLayer.new($tileset, @width, @height)]
      @layers += [TilemapLayer.new($tileset, @width, @height)]
      @layers += [@objects = ObjectLayer.new()]

      @editormap = EditorMap.new()
      @layers.each {|layer| @editormap.add_layer(layer.to_layer()) }
      
      # FIXME: Data might not get freed since its 'recursively' refcounted
      @editormap.set_metadata(make_metadata(self))
    end
  end

  def activate(workspace)
    $gui.workspace.set_map(@editormap)

    TilemapLayer.set_current(@layers[@current_layer])
    ObjectLayer.set_current(@objects)
  end

  def load(filename)
    print "Load Level to '", filename, "'\n"
    ## Insert your load code here
  end

  def save(filename)
    print "Save Level to '", filename, "'\n"
    ## Insert your save code here
  end
end

# EOF #
