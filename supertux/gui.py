# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


import subprocess
import os

from flexlay import (Color, ObjectBrush, Sprite, InputEvent,
                     ObjMapRectObject, ObjMapPathNode, Config,
                     ToolContext)
from flexlay.math import Point, Rect, Size
from flexlay.tools import (TilePaintTool, TileBrushCreateTool,
                           TileMapSelectTool, TileFillTool,
                           TileReplaceTool, ObjMapSelectTool,
                           ZoomTool, ZoomOutTool, WorkspaceMoveTool)

from .data import game_objects, create_gameobject
from .gameobj import PathNode
from .level import Level
from .sector import Sector
from .worldmap import WorldMap
from .worldmap_object import create_worldmapobject_at_pos  # worldmap_objects
from .tileset import SuperTuxTileset
from .button_panel import SuperTuxButtonPanel
from .menubar import SuperTuxMenuBar
from .toolbox import SuperTuxToolbox


BACKGROUND_LAYER = 1
INTERACTIVE_LAYER = 2
FOREGROUND_LAYER = 3


class SuperTuxGUI:

    current = None

    def __init__(self, flexlay):
        SuperTuxGUI.current = self

        self.use_worldmap = False

        self.tool_context = ToolContext()
        self.level = None
        self.sector = None

        self.gui = flexlay.create_gui_manager("SuperTux Editor")

        self.button_panel = SuperTuxButtonPanel(self.gui, self)
        self.toolbox = SuperTuxToolbox(self.gui, self)
        self.menubar = SuperTuxMenuBar(self.gui, self)

        self.display_properties = DisplayProperties()

        self.editor_map = self.gui.create_editor_map_component()
        self.statusbar = self.gui.create_statusbar()
        self.workspace = self.editor_map.get_workspace()

        # Tools
        self.workspace.set_tool(InputEvent.MOUSE_MIDDLE, WorkspaceMoveTool())

        self.minimap = self.gui.create_minimap(self.editor_map)

        self.objectselector = self.gui.create_object_selector(42, 42)
        self.editor_map.sig_drop.connect(self.on_object_drop)
        for objectdata in game_objects:
            sprite = Sprite.from_file(Config.current.datadir + objectdata[1])
            self.objectselector.add_brush(ObjectBrush(sprite, objectdata))

        self.tileselector = self.gui.create_tile_selector()
        self.tileselector.set_tileset(SuperTuxTileset.current)
        self.tileselector.add_tilegroup("All Tiles", SuperTuxTileset.current.get_tiles())
        for tilegroup in SuperTuxTileset.current.tilegroups:
            self.tileselector.add_tilegroup(tilegroup.name, tilegroup.tiles)

        self.layer_selector = self.gui.create_layer_selector()

        # self.worldmapobjectselector = self.gui.create_object_selector(42, 42)
        # if False:
        #     self.worldmapobjectselector.sig_drop.connect(self.on_worldmap_object_drop)
        # for obj in worldmap_objects:
        #     self.objectselector.add_brush(ObjectBrush(Sprite.from_file(Config.current.datadir + obj[1]),
        #                                                       obj[0]))

        # Loading Dialogs
        self.load_dialog = self.gui.create_openfiledialog("Load SuperTux Level")
        self.load_dialog.set_directory(Config.current.datadir + "levels/")
        self.save_dialog = self.gui.create_savefiledialog("Save SuperTux Level as...")
        self.save_dialog.set_directory(Config.current.datadir + "levels/")

        self.register_keyboard_shortcuts()

        # Popup menu
        # objmap_select_tool.sig_on_right_click().connect(proc{ | x, y |
        #              print("Launching Menu at %s, %s" % (x, y))
        #              menu=Menu(Point(x, y))
        #              menu.add_item(mysprite, "Delete Object(s)", proc{
        #                              print("Trying to delete
        #                              {self.workspace.get_map().metadata}
        #                              {self.workspace.get_map().metadata.objects}")
        #                              cmd=ObjectDeleteCommand(self.workspace.get_map().metadata.objects)
        #                              for i in objmap_select_tool.get_selection():
        #                                  cmd.add_object(i)
        #                              self.workspace.get_map().execute(cmd)
        #                              objmap_select_tool.clear_selection()
        #                            })
        #              menu.add_item(mysprite, "Edit Properties", proc{
        #                                for i in objmap_select_tool.get_selection():
        #                                  i.get_data().property_dialog()
        #                              }
        #                            })
        #              menu.run()
        #            })

        # setting initial state
        level = Level.from_size(100, 50)
        self.set_level(level, "main")

        self.set_tilemap_paint_tool()
        self.gui_show_foreground()

    def register_keyboard_shortcuts(self):
        self.editor_map.sig_on_key("f1").connect(lambda x, y: self.gui_toggle_minimap())
        self.editor_map.sig_on_key("m").connect(lambda x, y: self.gui_toggle_minimap())
        self.editor_map.sig_on_key("g").connect(lambda x, y: self.gui_toggle_grid())
        self.editor_map.sig_on_key("4").connect(lambda x, y: self.gui_toggle_display_props())

        self.editor_map.sig_on_key("3").connect(lambda x, y: self.gui_show_foreground())
        self.editor_map.sig_on_key("2").connect(lambda x, y: self.gui_show_interactive())
        self.editor_map.sig_on_key("1").connect(lambda x, y: self.gui_show_background())

        self.editor_map.sig_on_key("+").connect(lambda x, y: self.editor_map.zoom_in(Point(x, y)))
        self.editor_map.sig_on_key("-").connect(lambda x, y: self.editor_map.zoom_out(Point(x, y)))
        self.editor_map.sig_on_key("Enter").connect(lambda x, y: self.gui_set_zoom(1.0))

        self.editor_map.sig_on_key("i").connect(lambda x, y: self.insert_path_node(x, y))
        self.editor_map.sig_on_key("c").connect(lambda x, y: self.connect_path_nodes())

        self.editor_map.sig_on_key("7").connect(
            lambda x, y: self.workspace.get_map().metadata.parent.activate_sector("main",
                                                                                  self.workspace))
        self.editor_map.sig_on_key("8").connect(
            lambda x, y: self.workspace.get_map().metadata.parent.activate_sector("another_world",
                                                                                  self.workspace))

        self.editor_map.sig_on_key("e").connect(lambda x, y: self.gui_show_object_properties())

        def on_a_key(x, y):
            pos = self.editor_map.screen2world(Point(x, y))
            rectobj = ObjMapRectObject(Rect(pos,
                                            Size(128, 64)),
                                       Color(0, 255, 255, 155),
                                       None)
            self.workspace.get_map().metadata.objects.add_object(rectobj)

        self.editor_map.sig_on_key("a").connect(on_a_key)

    def on_worldmap_object_drop(self, brush, pos):
        pos = self.editor_map.screen2world(pos)
        object_type = brush.metadata
        create_worldmapobject_at_pos(
            self.workspace.get_map().metadata.objects, object_type, pos)

    def on_object_drop(self, brush, pos):
        create_gameobject(self.workspace.get_map(), self.workspace.get_map().metadata.objects, brush.metadata, pos, [])

    def run(self):
        self.gui.run()

    def show_objects(self):
        if False:  # GRUMBEL
            self.tileselector.show(False)
            if self.use_worldmap:
                self.objectselector.show(False)
            else:
                self.objectselector.show(True)

    def show_tiles(self):
        if False:  # GRUMBEL
            self.tileselector.show(True)
            self.objectselector.show(False)

    def show_none(self):
        if False:  # GRUMBEL
            self.tileselector.show(False)
            self.objectselector.show(False)

    def gui_show_foreground(self):
        self.display_properties.layer = FOREGROUND_LAYER
        self.display_properties.set(self.workspace.get_map().metadata)
        self.tool_context.tilemap_layer = self.workspace.get_map().metadata.foreground.tilemap_layer
        self.minimap.update_minimap()

    def gui_show_background(self):
        self.display_properties.layer = BACKGROUND_LAYER
        self.display_properties.set(self.workspace.get_map().metadata)
        self.tool_context.tilemap_layer = self.workspace.get_map().metadata.background.tilemap_layer
        self.minimap.update_minimap()

    def gui_show_interactive(self):
        self.display_properties.layer = INTERACTIVE_LAYER
        self.display_properties.set(self.workspace.get_map().metadata)
        self.tool_context.tilemap_layer = self.workspace.get_map().metadata.interactive.tilemap_layer
        self.minimap.update_minimap()

    def gui_show_all(self):
        self.display_properties.show_all = True
        self.display_properties.current_only = False
        self.display_properties.set(self.workspace.get_map().metadata)

    def gui_show_current(self):
        self.display_properties.show_all = False
        self.display_properties.current_only = False
        self.display_properties.set(self.workspace.get_map().metadata)

    def gui_show_only_current(self):
        self.display_properties.show_all = False
        self.display_properties.current_only = True
        self.display_properties.set(self.workspace.get_map().metadata)

    def gui_toggle_minimap(self):
        if self.minimap.get_widget().isVisible():
            self.minimap.get_widget().hide()
            self.button_panel.minimap_icon.set_up()
        else:
            self.minimap.get_widget().show()
            self.button_panel.minimap_icon.set_down()

    def gui_toggle_grid(self):
        tilemap = self.workspace.get_map().metadata.foreground.tilemap_layer
        tilemap.set_draw_grid(not tilemap.get_draw_grid())

        if tilemap.get_draw_grid():
            self.button_panel.grid_icon.set_down()
        else:
            self.button_panel.grid_icon.set_up()
        self.editor_map.editormap_widget.repaint()

    def gui_toggle_display_props(self):
        if self.display_properties.show_all:
            self.display_properties.show_all = False
        elif not(self.display_properties.current_only):
            self.display_properties.current_only = True
        else:
            self.display_properties.show_all = True
            self.display_properties.current_only = False

        self.display_properties.set(self.workspace.get_map().metadata)

    def gui_run_level(self):
        print("Run this level...")
        if self.use_worldmap:
            tmpfile = "/tmp/tmpflexlay-worldmap.stwm"
            self.save_level(tmpfile)
        else:
            # FIXME: use real tmpfile
            tmpfile = "/tmp/tmpflexlay-supertux.stl"
            self.save_level(tmpfile)
        subprocess.Popen([Config.current.binary, tmpfile])

    def gui_resize_sector(self):
        level = self.workspace.get_map().metadata
        dialog = self.gui.create_generic_dialog("Resize Sector")
        dialog.add_int("Width: ", level.width)
        dialog.add_int("Height: ", level.height)
        dialog.add_int("X: ", 0)
        dialog.add_int("Y: ", 0)

        def on_callback(w, h, x, y):
            print("Resize Callback")
            level.resize(Size(w, h), Point(x, y))

        dialog.set_callback(on_callback)

    def gui_smooth_level_struct(self):
        print("Smoothing level structure")
        tilemap = self.tool_context.tilemap_layer
        data = tilemap.get_data()
        # width = tilemap.width
        #
        # GRUMBEL
        # def get(x, y):
        #     return data[y * width + x]
        #
        # def set(x, y, val):
        #     data[y * width + x] = val
        #
        # def smooth(x, y):
        # pass  # GRUMBEL
        #     for ary in itile_conditions:
        #         if ((solid_itiles.index(get[x - 1, y - 1]) ? 1: 0) == ary[0]
        #             and (solid_itiles.index(get[x,  y - 1]) ? 1: 0) == ary[1]
        #             and (solid_itiles.index(get[x + 1, y - 1]) ? 1: 0) == ary[2]
        #             and (solid_itiles.index(get[x - 1, y]) ? 1: 0) == ary[3]
        #             and (solid_itiles.index(get[x,  y]) ? 1: 0) == ary[4]
        #             and (solid_itiles.index(get[x + 1, y]) ? 1: 0) == ary[5]
        #             and (solid_itiles.index(get[x - 1, y + 1]) ? 1: 0) == ary[6]
        #             and (solid_itiles.index(get[x,  y + 1]) ? 1: 0) == ary[7]
        #             and (solid_itiles.index(get[x + 1, y + 1]) ? 1: 0) == ary[8]):
        #             set[x, y, ary[9]]
        #
        # rect = self.tilemap_select_tool.get_selection_rect()
        #
        # start_x = rect.left
        # end_x = rect.right
        # start_y = rect.top
        # end_y = rect.bottom
        #
        # GRUMBEL
        # for y in range(start_y, end_y):
        #     for x in range(start_x, end_x):
        #         smooth(x, y)

        tilemap.set_data(data)

    def gui_resize_sector_to_selection(self):
        if self.tool_context.tile_selection is not None:
            level = self.workspace.get_map().metadata
            rect = self.tool_context.tile_selection.get_rect()
            if (rect.width > 2 and rect.height > 2):
                level.resize(rect.size, Point(-rect.left, -rect.top))

    def gui_edit_level(self):
        level = self.workspace.get_map().metadata.get_level()
        print(self.workspace.get_map())
        print(self.workspace.get_map().metadata)
        dialog = self.gui.create_generic_dialog("Edit Level")

        dialog.add_string("Name:", level.name)
        dialog.add_string("Author:", level.author)
        dialog.add_int("Target Time:", level.target_time)

        def on_callback(name, author):
            level.name = name
            level.author = author

        dialog.set_callback(on_callback)

    def gui_edit_sector(self):
        level = self.workspace.get_map().metadata.get_level()
        dialog = self.gui.create_generic_dialog("Edit Sector")

        dialog.add_string("Name: ", level.current_sector.name)
        dialog.add_string("Music: ", level.current_sector.music)
        dialog.add_float("Gravity: ", level.current_sector.gravity)

        def on_callback(name, music, gravity):
            level.current_sector.name = name
            level.current_sector.music = music
            level.current_sector.gravity = gravity

        dialog.set_callback(on_callback)

    def gui_zoom_in(self):
        factor = 2.0
        gc = self.editor_map.get_gc_state()
        zoom = gc.get_zoom()
        self.gui_set_zoom(zoom / pow(1.25, -factor))

    def gui_zoom_out(self):
        factor = 2.0
        gc = self.editor_map.get_gc_state()
        zoom = gc.get_zoom()
        self.gui_set_zoom(zoom * pow(1.25, -factor))

    def gui_zoom_fit(self):
        rect = self.workspace.get_map().get_bounding_rect()
        zoom = min(self.editor_map.editormap_widget.width() / rect.width,
                   self.editor_map.editormap_widget.height() / rect.height)
        self.gui_set_zoom(zoom, Point(rect.width / 2, rect.height / 2))

    def gui_set_zoom(self, zoom, pos=None):
        gc = self.editor_map.get_gc_state()
        pos = pos or gc.get_pos()
        gc.set_zoom(zoom)
        gc.set_pos(pos)
        self.editor_map.editormap_widget.repaint()

    def gui_remove_sector(self):
        sector = self.workspace.get_map().metadata
        sector.get_level().remove_sector(sector.name)

    def gui_add_sector(self):
        level = self.workspace.get_map().metadata.get_level()

        name = "sector"
        uniq_name = name
        i = 2
        while level.get_sectors().index(uniq_name):
            uniq_name = name + "<%d>" % i
            i += 1

        sector = Sector(level)
        sector.new_from_size(uniq_name, 30, 20)
        level.add_sector(sector)
        self.set_level(level, uniq_name)
        self.gui_edit_sector()

    def gui_show_object_properties(self):
        if self.tool_context.object_selection:
            selection = self.tool_context.object_selection
            if len(selection) > 1:
                print("Warning: Selection to large")
            elif len(selection) == 1:
                obj = selection[0].get_data()
                obj.property_dialog(self.gui)
            else:
                print("Warning: Selection is empty")

    def undo(self):
        self.workspace.get_map().undo()

    def redo(self):
        self.workspace.get_map().redo()

    def on_map_change(self):
        self.editor_map.editormap_widget.repaint()
        if self.workspace.get_map().undo_stack_size() > 0:
            self.button_panel.undo_icon.enable()
        else:
            self.button_panel.undo_icon.disable()

        if self.workspace.get_map().redo_stack_size() > 0:
            self.button_panel.redo_icon.enable()
        else:
            self.button_panel.redo_icon.disable()

    def gui_level_save_as(self):
        path = self.save_dialog.get_filename()
        if os.path.isdir(path):
            self.save_dialog.set_directory(path)
        else:
            self.save_dialog.set_directory(os.path.dirname(path) + "/")
        self.save_dialog.run(self.save_level)

    def gui_level_save(self):
        if self.use_worldmap:
            filename = self.workspace.get_map().metadata.filename
        else:
            filename = self.workspace.get_map().metadata.parent.filename

        print("Filename:", filename)
        if filename:
            self.save_dialog.set_directory(filename)
        else:
            filename = self.save_dialog.get_filename()
            if filename[-1] == "/"[0]:
                self.save_dialog.set_directory(filename)
            else:
                self.save_dialog.set_directory(os.path.dirname(filename) + "/")

        self.save_dialog.run(self.save_level)

    def gui_level_new(self):
        w, h = 100, 50
        self.new_level(w, h)

    def gui_level_load(self):
        self.load_dialog.run(self.load_level)

    def insert_path_node(self, x, y):
        print("Insert path Node")
        m = self.workspace.get_map().metadata
        pathnode = ObjMapPathNode(self.editor_map.screen2world(Point(x, y)),
                                  "PathNode")
        pathnode.metadata = PathNode(pathnode)
        m.objects.add_object(pathnode)

    def connect_path_nodes(self):
        print("Connecting path nodes")
        pathnodes = []
        for i in self.tool_context.object_selection:
            obj = i.get_data()
            if isinstance(obj, PathNode):
                pathnodes.append(obj.node)

        last = None
        for i in pathnodes:
            if last is not None:
                last.connect(i)
            last = i

    def gui_set_datadir(self):
        if os.path.isdir(Config.current.datadir):
            dialog = self.gui.create_generic_dialog("Specify the SuperTux data directory and restart")
            dialog.add_label("You need to specify the datadir where SuperTux is located")
            dialog.add_string("SuperTux datadir:", Config.current.datadir)

            def on_callback(datadir):
                Config.current.datadir = datadir

            dialog.set_callback(on_callback)

    def new_level(self, width, height):
        level = Level.from_size(width, height)
        self.set_level(level, "main")

    def load_level(self, filename):
        if filename[-5:] == ".stwm":
            self.load_worldmap(filename)
            return

        print("Loading: ", filename)
        level = Level.from_file(filename)
        self.set_level(level, "main")

        if filename not in Config.current.recent_files:
            Config.current.recent_files.append(filename)
            self.menubar.recent_files_menu.add_item(filename, lambda filename=filename: self.load_level(filename))

        self.minimap.update_minimap()

    def load_worldmap(self, filename):
        print("Loading: ", filename)
        worldmap = WorldMap(filename)
        worldmap.activate(self.workspace)

        if filename not in Config.current.recent_files:
            Config.current.recent_files.append(filename)
            self.menubar.recent_files_menu.add_item(filename, self.load_worldmap)

        self.minimap.update_minimap()
        self.use_worldmap = True

    def save_level(self, filename):
        if self.use_worldmap:
            level = self.workspace.get_map().metadata
        else:
            level = self.workspace.get_map().metadata.parent

        # Do backup save
        if os.path.isfile(filename):
            os.rename(filename, filename + "~")
        level.save(filename)
        level.filename = filename

    def raise_selection(self):
        for obj in self.tool_context.object_selection:
            self.workspace.get_map().metadata.objects.raise_object(obj)
        self.editor_map.editormap_widget.repaint()

    def lower_selection(self):
        for obj in self.tool_context.object_selection:
            self.workspace.get_map().metadata.objects.lower_object(obj)
        self.editor_map.editormap_widget.repaint()

    def raise_selection_to_top(self):
        selection = self.tool_context.object_selection
        self.workspace.get_map().metadata.objects.raise_objects_to_top(selection)
        self.editor_map.editormap_widget.repaint()

    def lower_selection_to_bottom(self):
        selection = self.tool_context.object_selection
        self.workspace.get_map().metadata.objects.lower_objects_to_bottom(selection)
        self.editor_map.editormap_widget.repaint()

    def set_tilemap_paint_tool(self):
        self.workspace.set_tool(InputEvent.MOUSE_LEFT, TilePaintTool())
        self.workspace.set_tool(InputEvent.MOUSE_RIGHT, TileBrushCreateTool())
        self.toolbox.set_down(self.toolbox.paint_icon)

    def set_tilemap_replace_tool(self):
        self.workspace.set_tool(InputEvent.MOUSE_LEFT, TileReplaceTool())
        self.workspace.set_tool(InputEvent.MOUSE_RIGHT, TileBrushCreateTool())
        self.toolbox.set_down(self.toolbox.replace_icon)

    def set_tilemap_fill_tool(self):
        self.workspace.set_tool(InputEvent.MOUSE_LEFT, TileFillTool())
        self.workspace.set_tool(InputEvent.MOUSE_RIGHT, TileBrushCreateTool())
        self.toolbox.set_down(self.toolbox.fill_icon)

    def set_tilemap_select_tool(self):
        self.workspace.set_tool(InputEvent.MOUSE_LEFT, TileMapSelectTool())
        self.workspace.set_tool(InputEvent.MOUSE_RIGHT, None)
        self.toolbox.set_down(self.toolbox.select_icon)

    def set_zoom_tool(self):
        self.workspace.set_tool(InputEvent.MOUSE_LEFT, ZoomTool())
        self.workspace.set_tool(InputEvent.MOUSE_RIGHT, ZoomOutTool())
        self.toolbox.set_down(self.toolbox.zoom_icon)

    def set_objmap_select_tool(self):
        self.workspace.set_tool(InputEvent.MOUSE_LEFT, ObjMapSelectTool())
        self.workspace.set_tool(InputEvent.MOUSE_RIGHT, None)
        self.toolbox.set_down(self.toolbox.object_icon)

    def set_level(self, level, sectorname):
        self.level = level
        for sec in self.level.sectors:
            if sec.name == sectorname:
                self.set_sector(sec)
                break

    def set_sector(self, sector):
        self.sector = sector

        self.workspace.set_map(self.sector.editormap)
        self.layer_selector.set_map(self.sector.editormap)

        ToolContext.current.tilemap_layer = self.sector.interactive.tilemap_layer
        ToolContext.current.object_layer = self.sector.objects

        self.sector.editormap.sig_change.connect(SuperTuxGUI.current.on_map_change)


class DisplayProperties:

    def __init__(self):
        self.layer = INTERACTIVE_LAYER
        self.show_all = False
        self.current_only = False

    def set(self, sector):
        if sector is None or not isinstance(sector, Sector):
            return

        if self.current_only:
            active = Color(255, 255, 255)
            deactive = Color(0, 0, 0, 10)
        else:
            active = Color(255, 255, 255)
            deactive = Color(150, 150, 250, 150)

        if self.show_all:
            sector.foreground.tilemap_layer.set_foreground_color(active)
            sector.interactive.tilemap_layer.set_foreground_color(active)
            sector.background.tilemap_layer.set_foreground_color(active)
        else:
            if (self.layer == FOREGROUND_LAYER):
                sector.foreground.tilemap_layer.set_foreground_color(active)
            else:
                sector.foreground.tilemap_layer.set_foreground_color(deactive)

            if (self.layer == INTERACTIVE_LAYER):
                sector.interactive.tilemap_layer.set_foreground_color(active)
            else:
                sector.interactive.tilemap_layer.set_foreground_color(deactive)

            if (self.layer == BACKGROUND_LAYER):
                sector.background.tilemap_layer.set_foreground_color(active)
            else:
                sector.background.tilemap_layer.set_foreground_color(deactive)


# EOF #
