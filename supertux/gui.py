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


import os
import subprocess
import logging
import tempfile
import threading

from PyQt4.QtGui import (QIcon, QMessageBox)

from flexlay import (Color, InputEvent, ObjMapRectObject, ObjMapTilemapObject,
                     ObjMapPathNode, Config, ToolContext, ObjectAddCommand,
                     Workspace, TilemapLayer)
from flexlay.gui.file_dialog import OpenFileDialog, SaveFileDialog
from flexlay.math import Point, Rect, Size
from flexlay.tools import (TilePaintTool, TileBrushCreateTool,
                           TileMapSelectTool, TileFillTool,
                           TileReplaceTool, ObjMapSelectTool,
                           ZoomTool, ZoomOutTool, WorkspaceMoveTool)
from .button_panel import SuperTuxButtonPanel
from .gameobj import PathNode, Tux
from .gameobj_factor import supertux_gameobj_factory
from .level import Level
from .menubar import SuperTuxMenuBar
from .new_level import NewLevelWizard
from .new_addon import NewAddonWizard
from .sector import Sector
from .tileset import SuperTuxTileset
from .tilemap import SuperTuxTileMap
from .toolbox import SuperTuxToolbox
from .supertux_arguments import SuperTuxArguments
from .level_file_dialog import OpenLevelFileDialog, SaveLevelFileDialog
from .addon_dialog import SaveAddonDialog

BACKGROUND_LAYER = 1
INTERACTIVE_LAYER = 2
FOREGROUND_LAYER = 3


class SuperTuxGUI:
    current = None

    def __init__(self, flexlay):
        SuperTuxGUI.current = self
        supertux_gameobj_factory.supertux_gui = self

        self.use_worldmap = False

        self.tool_context = ToolContext()
        self.level = None
        self.sector = None

        self.gui = flexlay.create_gui_manager("SuperTux Editor")
        self.gui.window.setWindowIcon(QIcon("data/images/supertux/supertux-editor.png"))
        self.gui.window.set_on_close(self.on_window_close)

        self.button_panel = SuperTuxButtonPanel(self.gui, self)
        self.toolbox = SuperTuxToolbox(self.gui, self)

        self.editor_map = self.gui.create_editor_map_component()
        self.statusbar = self.gui.create_statusbar()
        self.workspace = self.editor_map.get_workspace()

        # Tools
        self.workspace.set_tool(InputEvent.MOUSE_MIDDLE, WorkspaceMoveTool())

        self.minimap = self.gui.create_minimap(self.editor_map)

        self.objectselector = self.gui.create_object_selector(42, 42)
        self.properties_widget = self.gui.create_properties_view()

        self.editor_map.sig_drop.connect(self.on_object_drop)
        for object_brush in supertux_gameobj_factory.create_object_brushes():
            self.objectselector.add_brush(object_brush)

        self.tileselector = self.gui.create_tile_selector()
        self.gui_set_tileset(SuperTuxTileset.current)

        self.layer_selector = self.gui.create_layer_selector(self.generate_tilemap_obj)

        # self.worldmapobjectselector = self.gui.create_object_selector(42, 42)
        # if False:
        #     self.worldmapobjectselector.sig_drop.connect(self.on_worldmap_object_drop)
        # for obj in worldmap_objects:
        #     self.objectselector.add_brush(ObjectBrush(Sprite.from_file(os.path.join(Config.current.datadir, obj[1])),
        #                                                       obj[0]))

        # Loading Dialogs
        self.load_dialog = OpenLevelFileDialog("Load SuperTux Level")
        self.load_dialog.set_directory(Config.current.datadir, "levels")
        self.save_dialog = SaveLevelFileDialog("Save SuperTux Level As...")
        self.save_dialog.set_directory(Config.current.datadir, "levels")
        self.addon_save_dialog = SaveAddonDialog("Save SuperTux Add-on As...")
        self.addon_save_dialog.set_directory(Config.current.datadir, "addons")

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
        #                                  i.metadata.property_dialog()
        #                              }
        #                            })
        #              menu.run()
        #            })

        # setting initial state
        level = Level.from_size(100, 50)
        self.set_level(level, "main")

        self.set_tilemap_paint_tool()

        # Must be after LayerSelector initialised
        self.menubar = SuperTuxMenuBar(self.gui, self)

        # Command line arguments, when game is run
        self.arguments = SuperTuxArguments()

    def register_keyboard_shortcuts(self):
        self.editor_map.sig_on_key("f1").connect(lambda x, y: self.gui_toggle_minimap())
        self.editor_map.sig_on_key("m").connect(lambda x, y: self.gui_toggle_minimap())
        self.editor_map.sig_on_key("g").connect(lambda x, y: self.gui_toggle_grid())

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

        self.editor_map.sig_on_key("p").connect(lambda x, y: self.gui_show_object_properties())

        def on_a_key(x, y):
            pos = self.editor_map.screen2world(Point(x, y))
            rectobj = ObjMapRectObject(Rect(pos,
                                            Size(128, 64)),
                                       Color(0, 255, 255, 155),
                                       None)
            self.workspace.get_map().metadata.objects.add_object(rectobj)

        self.editor_map.sig_on_key("a").connect(on_a_key)

    def on_window_close(self, *args):
        """Called when window x button is clicked

        Ask whether to save, continue, or just quit.
        :return: boolean whether to close or not. If not boolean, will close.
        """
        editor_map = Workspace.current.get_map()
        # If the most recent save was the same as the save_pointer index,
        # we can safely quit
        if editor_map.save_pointer == len(editor_map.undo_stack):
            return True
        else:
            choice = QMessageBox.warning(self.gui.window, "Unsaved Changes to Level",
                                         "The level has been changed since "
                                         "the last save.",
                                         "Save Now", "Cancel", "Leave Anyway",
                                         0, 1)
            if choice == 0:
                dialog_is_cancelled = False

                def after_save(i):
                    dialog_is_cancelled = (i == 0)

                self.save_dialog.file_dialog.finished.connect(after_save)
                self.gui_level_save()
                # If saved, show confirmation dialog to reassure user.
                if not dialog_is_cancelled:
                    QMessageBox.information(self.gui.window, "Saved Successfully", "Editor will now quit")
                # If dialog is cancelled, don't quit, as that would lose changes
                return not dialog_is_cancelled
            elif choice == 1:
                return False
            elif choice == 2:
                return True

    def on_object_drop(self, brush, pos):
        obj = supertux_gameobj_factory.create_gameobj_at(brush.metadata, pos)
        if obj is None:
            logging.error("Unknown object type dropped: %r" % brush.metadata)
        else:
            cmd = ObjectAddCommand(self.workspace.get_map().metadata.object_layer)
            cmd.add_object(obj.objmap_object)
            self.workspace.get_map().execute(cmd)

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

    def gui_toggle_minimap(self):
        if self.minimap.get_widget().isVisible():
            self.minimap.get_widget().hide()
            self.button_panel.minimap_icon.set_up()
        else:
            self.minimap.get_widget().show()
            self.button_panel.minimap_icon.set_down()

    def gui_toggle_grid(self):
        self.workspace.get_map().draw_grid = not self.workspace.get_map().draw_grid

        if not self.workspace.get_map().draw_grid:
            self.button_panel.grid_icon.set_down()
        else:
            self.button_panel.grid_icon.set_up()
        self.editor_map.editormap_widget.repaint()

    def gui_set_tileset(self, tileset):
        self.tileselector.set_tileset(tileset)
        self.tileselector.clear_tilegroups()
        self.tileselector.add_tilegroup("All Tiles", tileset.get_tiles())
        for tilegroup in tileset.tilegroups:
            self.tileselector.add_tilegroup(tilegroup.name, tilegroup.tiles)

        if self.sector is not None:
            for tilemap_layer in [tilemap.tilemap_layer for tilemap in self.sector.tilemaps]:
                tilemap_layer.tileset = tileset

            self.editor_map.editormap_widget.repaint()

        if self.level is not None:
            self.level.tileset_path = tileset.filename

    def gui_change_tileset(self):
        tileset_dialog = OpenFileDialog("Select Tileset To Open", ("SuperTux Tilesets (*.strf)", "All Files (*)"))
        tileset_dialog.set_directory(Config.current.datadir, "images")
        tileset_dialog.run(self.set_tileset)

    def set_tileset(self, filename):
        """Set tileset from (.strf) filename"""
        if not filename:
            QMessageBox.warning(None, "No Tileset Selected", "No tileset was selected, aborting...")
        tileset = SuperTuxTileset(32)
        tileset.load(filename)
        self.gui_set_tileset(tileset)

    def gui_run_level(self):
        logging.info("Run this level...")

        level = self.workspace.get_map().metadata.get_level()
        if level.is_worldmap:
            suffix = ".stwm"
        else:
            suffix = ".stl"

        tmpfile = tempfile.mkstemp(suffix=suffix, prefix="flexlay-")
        self.save_level(tmpfile[1], False, True)

        self.arguments.run_level = tmpfile[1]

        # for obj in self.sector.object_layer.get_objects():
        #     if isinstance(obj.metadata, Tux):
        #         self.arguments.spawn_at = obj.pos

        try:
            thread = threading.Thread(target=self.gui_run_level_thread,
                                      args=(self.gui_run_level_cleanup,
                                            self.arguments.get_popen_arg(),
                                            tmpfile))
            thread.start()
        except FileNotFoundError:
            QMessageBox.warning(None, "No Supertux Binary Found",
                                "Press OK to select your Supertux binary")
            Config.current.binary = OpenFileDialog("Open Supertux Binary").filename
            if not Config.current.binary:
                raise RuntimeError("binary path missing, use --binary BIN")

        # self.arguments.spawn_at = None

    def gui_run_level_thread(self, postexit_fn, popen_args, tmpfile):
        subproc = subprocess.Popen(popen_args)
        subproc.wait()
        postexit_fn(tmpfile)
        return

    def gui_run_level_cleanup(self, tmpfile):
        # Safely get rid of temporary file
        os.close(tmpfile[0]) # Close file descriptor
        os.remove(tmpfile[1]) # Remove the file

    def gui_record_level(self):
        self.arguments.record_demo_file = SaveFileDialog("Choose Record Target File")
        self.gui_run_level()
        self.arguments.record_demo_file = None

    def gui_play_demo(self):
        QMessageBox.information(None,
                                "Select a level file",
                                "You must now select a level file - the level of the demo")
        level = OpenLevelFileDialog("Select The Level")
        QMessageBox.information(None,
                                "Select a demo file",
                                "You must now select a demo file to play")
        demo = OpenFileDialog("Select The Demo File To Play").filename

        self.arguments.play_demo_file = demo

        subprocess.Popen(self.arguments.get_popen_arg())

        self.arguments.play_demo_file = None

    def gui_watch_example(self):
        level = os.path.join(Config.current.datadir, "levels", "world1", "01 - Welcome to Antarctica.stl")
        demo = os.path.join("data", "supertux", "demos", "karkus476_plays_level_1")
        subprocess.Popen([Config.current.binary, level, "--play-demo", demo])

    def gui_resize_sector(self):
        level = self.workspace.get_map().metadata
        dialog = self.gui.create_generic_dialog("Resize Sector")
        dialog.add_int("Width: ", level.width)
        dialog.add_int("Height: ", level.height)
        dialog.add_int("X: ", 0)
        dialog.add_int("Y: ", 0)

        def on_callback(w, h, x, y):
            logging.info("Resize Callback")
            level.resize(Size(w, h), Point(x, y))

        dialog.set_callback(on_callback)

    def gui_smooth_level_struct(self):
        logging.info("Smoothing level structure")
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
        dialog = self.gui.create_generic_dialog("Edit Level")

        dialog.add_string("Name:", level.name)
        dialog.add_string("Author:", level.author)
        dialog.add_string("Contact:", level.contact)
        dialog.add_int("Target Time:", level.target_time)

        def on_callback(name, author, contact, target_time):
            level.name = name
            level.author = author
            level.contact = contact
            level.target_time = target_time

        dialog.set_callback(on_callback)

    def gui_edit_sector(self):
        level = self.workspace.get_map().metadata.get_level()
        dialog = self.gui.create_generic_dialog("Edit Sector")

        dialog.add_string("Name: ", level.current_sector.name)
        dialog.add_file("Music: ", level.current_sector.music,
                        ret_rel_to=Config.current.datadir,
                        show_rel_to=os.path.join(Config.current.datadir, "music"),
                        open_in=os.path.join(Config.current.datadir, "music"))
        dialog.add_float("Gravity: ", level.current_sector.gravity)

        def on_callback(*args):
            level.current_sector.name = args[0]
            level.current_sector.music = args[1]
            level.current_sector.gravity = args[2]

        dialog.add_callback(on_callback)

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
                logging.warning("Selection too large")
            elif len(selection) == 1:
                obj = selection[0].metadata
                obj.property_dialog(self.gui.window)
            else:
                logging.warning("Selection is empty")

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

        logging.info("Save Filename: " + filename)
        if filename:
            self.save_level(filename)
        else:
            filename = self.save_dialog.get_filename()
            if filename[-1] == "/"[0]:
                self.save_dialog.set_directory(filename)
            else:
                self.save_dialog.set_directory(os.path.dirname(filename) + "/")

            self.save_dialog.run(self.save_level)

    def gui_level_new(self):
        dialog = NewLevelWizard(self.gui.window)
        dialog.exec_()

        if dialog.level is not None:
            def save_path_chosen(save_path):
                dialog.level.save(save_path)
                self.load_level(save_path)

            self.save_dialog.run(save_path_chosen)

        # Does nothing:
        self.new_level()

    def gui_addon_new(self):
        dialog = NewAddonWizard(self.gui.window)
        dialog.exec_()
        if dialog.addon is not None:
            def save_path_chosen(save_path):
                dialog.addon.save(save_path)
                self.load_addon(dialog.addon, save_path)
            self.addon_save_dialog.run(save_path_chosen)
        pass

    def gui_level_load(self):
        self.load_dialog.run(self.load_level)

    def insert_path_node(self, x, y):
        logging.info("Insert path Node")
        m = self.workspace.get_map().metadata
        pathnode = ObjMapPathNode(self.editor_map.screen2world(Point(x, y)),
                                  "PathNode")
        pathnode.metadata = PathNode(pathnode)
        m.objects.add_object(pathnode)

    def connect_path_nodes(self):
        logging.info("Connecting path nodes")
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

    def new_level(self):
        pass

    def load_level(self, filename, set_title=True):
        logging.info("Loading: " + filename)

        # Clear object selection, it's a new level!
        self.tool_context.object_selection.clear()

        self.gui.properties_widget.clear_properties()

        # Set title if desired
        if set_title:
            self.gui.window.setWindowTitle("SuperTux Editor: [" + filename + "]")

        level = Level.from_file(filename)

        if level.tileset_path != SuperTuxTileset.current.filename:
            tileset = SuperTuxTileset(32)
            tileset.load(os.path.join(Config.current.datadir, level.tileset_path))
            self.gui_set_tileset(tileset)

            # Tileset has changed, reload level:
            level = Level.from_file(filename)

        self.set_level(level, "main")

        Config.current.add_recent_file(filename)
        self.menubar.update_recent_files()

        self.minimap.update_minimap()
        # TODO: We don't yet support multiple sectors, so we set the first sector's name.
        self.editor_map.set_sector_tab_label(0, level.sectors[0].name)

    def save_level(self, filename, set_title=True, is_tmp=False):
        if set_title:
            self.gui.window.setWindowTitle("SuperTux Editor: [" + filename + "]")

        editor_map = Workspace.current.get_map()
        editor_map.save_pointer = len(editor_map.undo_stack)

        level = self.workspace.get_map().metadata.parent

        Config.current.add_recent_file(filename)
        self.menubar.update_recent_files()

        # Do backup save if the file exists and is going to be saved permanently.
        if os.path.isfile(filename) and not is_tmp:
            os.rename(filename, filename + "~")
        level.save(filename)
        level.filename = filename

    def load_addon(self, addon, dirname):
        print("Add-on dirname is: " + dirname)
        self.gui.project_widget.set_addon()
        self.gui.project_widget.set_project_directory(dirname)
        pass

    def load_addon_zip(self, filename):
        print("Add-on zip path is: " + zip_path)
        pass

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
        self.workspace.set_tool(InputEvent.MOUSE_LEFT, ObjMapSelectTool(self.gui))
        self.workspace.set_tool(InputEvent.MOUSE_RIGHT, ObjMapSelectTool(self.gui))
        self.toolbox.set_down(self.toolbox.object_icon)

    def set_level(self, level, sectorname):
        self.level = level
        for sec in self.level.sectors:
            if sec.name == sectorname:
                self.set_sector(sec)
                break

    def set_sector(self, sector):
        self.sector = sector
        self.workspace.current_sector = sector

        self.workspace.set_map(self.sector.editormap)
        self.layer_selector.set_map(self.sector.editormap)
        # TODO: We don't yet support multiple sectors, so we set the first sector's name.
        self.editor_map.set_sector_tab_label(0, sector.name)

        ToolContext.current.tilemap_layer = self.sector.get_some_solid_tilemap().tilemap_layer
        ToolContext.current.object_layer = self.sector.object_layer

        self.sector.editormap.sig_change.connect(SuperTuxGUI.current.on_map_change)

    def generate_tilemap_obj(self):
        """Generate a basic ObjMapTilemapObject with basic parameters

        May later open a dialog.

        :return: ObjMapTilemapObject
        """
        tilemap = SuperTuxTileMap.from_size(self.sector.width,
                                            self.sector.height,
                                            "<no name>",
                                            0, True)
        tilemap_object = ObjMapTilemapObject(tilemap.tilemap_layer, tilemap)
        return tilemap_object

    def camera_properties(self):
        self.sector.camera.property_dialog(self.gui.window)


# EOF #
