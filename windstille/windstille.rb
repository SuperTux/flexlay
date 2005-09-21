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

## A basic tile editor that should act as example for other games, use
## it to fork your own code.

## First we try to read a config file to set some variables
## Load Flexlay library
require "flexlay_wrap"
include Flexlay_wrap

require "scanf.rb"
require "flexlay.rb"

require "controller.rb"
require "sexpr.rb"
require "command_line.rb"
require "sexpr_config_file.rb"
require "gui.rb"
require "sector.rb"
require "tileset.rb"

def parse_command_line()
  cmd = CommandLine.new() {
    name("Windstille Editor V0.1")
    usage("[OPTION]... [FILE]...")
    description("Editor for editing Windstille map files.")
    
    group("Display")
    option(?f, "fullscreen", nil,            "Launch in fullscreen mode")
    option(?w, "window",     nil,            "Launch in window mode")
    option(?g, "geometry",   "WIDTHxHEIGHT", "Launch in the given resolution")

    group("Misc")
    option(?d, "datadir",    "DIR",          "Set the datadir to use")
    option(?h, "help",       nil,            "Print this help")

    text("Report bugs to <grumbel@gmx.de>.")
  }

  begin
    args = cmd.parse(ARGV)
  rescue CommandLineException => err
    puts('windstille-editor:' + err)
    exit()
  end

  args.each { |option, argument|
    case option
    when ?w
      $config.set("fullscreen", false)
      
    when ?f
      $config.set("fullscreen", true)
      
    when ?d
      $config.set("datadir", argument)

    when ?g
      (width, height) = argument.scanf("%dx%d")
      $config.set("screen-width",  width)  if width
      $config.set("screen-height", height) if height
      
    when ?h
      cmd.print_help()
      cmd.exit()
      exit()

    else
      raise "Bug: Unhandled option: -#{option} #{argument}"
    end
  }
end

$config = SExprConfigFile.new("windstille-editor") {
  register("datadir",       nil)
  register("screen-width",  800)
  register("screen-height", 600)
  register("fullscreen",    false)
  register("recent-files",  [])
}

parse_command_line()

$datadir       = $config.get("datadir")
$fullscreen    = $config.get("fullscreen")

$screen_width  = $config.get("screen-width")
$screen_height = $config.get("screen-height")

## Init Flexlay itself
$flexlay = Flexlay.new()
$flexlay.init($screen_width, $screen_height, $fullscreen, true)

$gui = GUI.new()

## Initialize Tools and stuff
def init()
  $controller = Controller.new()
  
  $tileset = Tileset.new(32)
  $tileset.load($datadir + "tiles.scm")
  
  $gui.post_initalize()

  if not $levelfile then
    Sector.new(100, 30).activate($workspace)
  else
    $controller.load_level($levelfile)
  end

  $gui.workspace.set_tool($controller.tilemap_paint_tool.to_tool());
end

if $datadir == nil or not File.exist?($datadir) then
  dialog = GenericDialog.new("Windstille Data Directory", $gui.gui.get_component())
  dialog.add_label("You need to specify the datadir of Windstille is located")
  dialog.add_string("Datadir:", $datadir || "")
  
  dialog.set_block { |datadir|
    $datadir = datadir 
    # datadir is ready, so startup
    init()
  }
else
  # datadir is ready, so startup
  init()
end

begin # Run user code if available
  home = ENV['HOME']
  if home then
    user_rb = home + "/.windstille-editor/user.rb"
  else 
    user_rb = "user.rb"
  end

  if File.exists?(user_rb) then
      require user_rb
  end
end

$gui.run()

$config.set("datadir", $datadir)
$config.set("recent-files", $controller.recent_files)
$config.write()

# $flexlay.deinit()

# EOF #
