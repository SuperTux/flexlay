#!/usr/bin/ruby
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
require "sexpr.rb"
require "animation.rb"
require "image.rb"
require "gui.rb"

flexlay = Flexlay.new()

# $screen_rect = CL_Rect.new(CL_Point.new(0, 0), CL_Size.new(800, 600))
$screen_rect = CL_Rect.new(CL_Point.new(0, 0), CL_Size.new(1024, 768))

flexlay.init($screen_rect.get_width(), $screen_rect.get_height(), false)

$sketch_stroke_tool  = SketchStrokeTool.new()
$layer_move_tool     = LayerMoveTool.new()
$zoom_tool           = ZoomTool.new()
$objmap_select_tool  = ObjMapSelectTool.new()

DrawerProperties.current().set_color(CL_Color.new(0, 0, 0, 50))

$gui   = PaintGUI.new()

$animation = Animation.new()
$image = $animation.get_current_image()
# $image.add_layer("/tmp/img.png")
# $image.layers[0].set_pixeldata(make_pixelbuffer("/tmp/img.png"))
# BitmapLayer.set_current($image.layers[0])

$image.activate($gui.workspace)

# drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())

if true then
  DrawerProperties.current().set_brush(GeneratedBrush.new(BRUSH_SHAPE_CIRCLE, 
                                                          32,  # radius
                                                          2,   # spikes
                                                          0.75, # hardness
                                                          1.0, # aspect
                                                          0).to_brush()) # angle
else
  DrawerProperties.current().set_brush(SpriteBrush.new(make_sprite("../data/images/brush/brush8.png")).to_brush)
end

$image.layers_count.times {|i|
  button = CL_Button.new(CL_Rect.new(CL_Point.new(25*i+6, 450), CL_Size.new(25, 25)), "#{i}",
                         $gui.selector_window)
  connect(button.sig_clicked(), proc{ $image.set_active_layer(i) })
}

$gui.run()

# FIXME: Can't deinit flexlay, since we would crash then, refcouting throuble...
# flexlay.deinit()
# puts "And now we crash"

# EOF #
