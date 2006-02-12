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

require "flexlay_wrap"
include Flexlay_wrap

require "flexlay.rb"
require "tileset.rb"
require "gui.rb"
require "level.rb"
require "controller.rb"

class SuperTuxPortableEditor
  def initialize(args)
    @levelfile = nil

    init_flexlay()
  end

  def init_flexlay()
    $screen_width  = 800
    $screen_height = 600
    $fullscreen    = false

    ## Init Flexlay itself
    $flexlay = Flexlay.new()
    $flexlay.init("SuperTux Portable Editor V0.1", $screen_width, $screen_height, $fullscreen, true)

    $controller = Controller.new()

    $datadir = "/home/ingo/projects/supertux/trunk/supertux-portable/data/"
    $tileset = Tileset.new(8)
    $tileset.load($datadir + "antarctica.stpts")

    $gui = GUI.new()
    
    if ARGV.length == 0 then
      Level.new_from_size(256, 32).activate($gui.workspace)
    else
      Level.new_from_file(ARGV[0]).activate($gui.workspace)
    end
  end

  def run()
    $gui.run()
    # $flexlay.deinit()
  end

end

# EOF #
