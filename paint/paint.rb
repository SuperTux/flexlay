#!/usr/bin/ruby

# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2004 Ingo Ruhnke <grumbel@gmail.com>
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

require "flexlay.rb"
require "sexpr.rb"

require_relative "animation.rb"
require_relative "gui.rb"
require_relative "image.rb"

$flexlay_datadir = "../data"

flexlay = Flexlay.new()

if ENV["FLEXLAY_DATADIR"] then
  $flexlay_datadir = ENV["FLEXLAY_DATADIR"]
  flexlay.set_datadir($flexlay_datadir)
end

# $screen_rect = CL_Rect.new(CL_Point.new(0, 0), CL_Size.new(800, 600))
$screen_rect = CL_Rect.new(CL_Point.new(0, 0), CL_Size.new(1024, 768))

flexlay.init("Flexlay Paint", $screen_rect.get_width(), $screen_rect.get_height(), false)

$sketch_stroke_tool  = SketchStrokeTool.new()
$workspace_move_tool = WorkspaceMoveTool.new()
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

if false then
  DrawerProperties.current().set_brush(GeneratedBrush.new(BRUSH_SHAPE_CIRCLE, 
                                                          32,  # radius
                                                          2,   # spikes
                                                          0.75, # hardness
                                                          1.0, # aspect
                                                          0).to_brush()) # angle
else
  DrawerProperties.current().set_brush(SpriteBrush.new(make_sprite($flexlay_datadir + "/images/brush/brush8.png")).to_brush)
end

$image.layers_count.times {|i|
  button = CL_Button.new(CL_Rect.new(CL_Point.new(25*i+6, 450), CL_Size.new(25, 25)), "#{i}",
                         $gui.selector_window)
  connect_cl(button.sig_clicked(), proc{ $image.set_active_layer(i) })
}

$gui.run()

# FIXME: Can't deinit flexlay, since we would crash then, refcouting throuble...
# flexlay.deinit()
# puts "And now we crash"

# EOF #
