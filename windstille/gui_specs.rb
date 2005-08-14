##  $Id$
##   ______ __               ___
##  |   ___|  |.-----.--.--.|   | .---.-.--.--.
##  |   ___|  ||  -__|_   _||   |_|  _  |  |  |
##  |__|   |__||_____|__.__||_____|___._|___  |
##                                      |_____|
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
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
##  02111-1307, USA.

$menu_spec = [
  ["File",
    ["Open...",    proc{ $gui.level_load() }],
    ["Save...",    proc{ $gui.level_save() }],
    ["Save As...", proc{ $gui.level_save_as() }],
    ["Quit",       proc{ $gui.gui.quit() }],
  ],
  ["Edit",
    ["Resize", proc{ $gui.resize_level() }],
    ["Resize to selection", proc{ $gui.resize_level_to_selection()}],
    ["Debug Shell", proc{ $gui.run_python()}],
  ],
  ["Zoom",
    ["1:4 (25%)",   proc{ $gui.set_zoom(0.125) }],
    ["1:2 (50%) ",  proc{ $gui.set_zoom(0.25) }],
    ["1:1 (100%) ", proc{ $gui.set_zoom(0.5) }],
    ["2:1 (200%) ", proc{ $gui.set_zoom(1.0) }],
    ["4:1 (400%) ", proc{ $gui.set_zoom(2.0) }],
  ]
]

$buttonpanel_spec = [
  [:icon,       "new",         "../data/images/icons24/stock_new.png",     proc{ $gui.new_level() }],
  [:icon,       "open",        "../data/images/icons24/stock_open.png",    proc{ $gui.level_load() }],
  [:small_icon, "recent",      "../data/images/icons24/downarrow.png",     proc{ $controller.recent_files_menu.run() }],
  [:icon,       "save",        "../data/images/icons24/stock_save.png",    proc{ $gui.level_save() }],
  [:icon,       "saveas",      "../data/images/icons24/stock_save_as.png", proc{ $gui.level_save_as() }],
  [:seperator],
  [:icon,       "copy",        "../data/images/icons24/stock_copy.png",    nil],
  [:icon,       "paste",       "../data/images/icons24/stock_paste.png",   nil],
  [:seperator],
  [:icon,       "undo",        "../data/images/icons24/stock_undo.png",    proc{ $gui.workspace.get_map().undo() }],
  [:icon,       "redo",        "../data/images/icons24/stock_redo.png",    proc{ $gui.workspace.get_map().redo() }],
  [:seperator],
  [:toggle,     "grid",        "../data/images/icons24/grid.png",          proc{ $gui.toggle_grid() }],
  [:seperator],
  [:icon,       "background",  "../data/images/icons24/background.png",    proc{ $gui.show_layer(:background) }],
  [:icon,       "interactive", "../data/images/icons24/interactive.png",   proc{ $gui.show_layer(:interactive) }],
  [:icon,       "foreground",  "../data/images/icons24/foreground.png",    proc{ $gui.show_layer(:foreground) }],
  [:icon,       "viewprops",   "../data/images/icons24/eye.png",           proc{ $gui.layer_menu.run() }],
  [:seperator],
  [:icon,       "tilegroups",  "../data/images/icons24/eye.png",           proc{ $gui.tilegroup_menu.run() }],
]

$toolbar_spec = [
  [:icon, "paint",  "../data/images/tools/stock-tool-pencil-22.png",      proc{ $controller.set_tilemap_paint_tool() }],
  [:icon, "select", "../data/images/tools/stock-tool-rect-select-22.png", proc{ $controller.set_tilemap_select_tool() }],
  [:icon, "zoom",   "../data/images/tools/stock-tool-zoom-22.png",        proc{ $controller.set_zoom_tool() }],
  [:icon, "object", "../data/images/tools/stock-tool-clone-22.png",       proc{ $controller.set_objmap_select_tool() }],
]

$keybinding_spec = [
  ["f1", proc{ |x, y| $gui.toggle_minimap()}],
  ["m",  proc{ |x, y| $gui.toggle_minimap()}],
  ["g",  proc{ |x, y| $gui.toggle_grid()}],
  ["4",  proc{ |x, y| $gui.toggle_display_props()}],
  ["3",  proc{ |x, y| $gui.show_layer(:foreground)}],
  ["2",  proc{ |x, y| $gui.show_layer(:interactive)}],
  ["1",  proc{ |x, y| $gui.show_layer(:background)}],
]

$guilayout_spec = [
  :vbox, 
  [:homogenus, false],
  [:components,
    
    [:menubar,     
      [:name,   'menubar'], 
      [:size, 23],
      [:spec,   $menu_spec]],

    [:buttonpanel, 
      [:name,   'buttonpanel'],
      [:size, 33], 
      [:spec,   $buttonpanel_spec]],

    [:hbox, 
      [:components,

        [:editormap, 
          [:name, 'editormap']],

        [:panel,
          [:layout, :vbox],
          [:size, 192],
          [:components,
            [:tab,
              [:components,
                [:tileselector,
                  [:name, 'tileselector']],
                [:objectselector,
                  [:name, 'objectselector']]]],
            [:minimap, 
              [:name,    'minimap'],
              [:size,    144],
              [:padding, 5],
            ]]]
      ]]]]

## EOF ##
