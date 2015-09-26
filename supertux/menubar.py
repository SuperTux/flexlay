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


from flexlay import Config


class SuperTuxMenuBar:

    def __init__(self, gui_manager, editor):
        self.gui_manager = gui_manager

        # Create Menu
        self.menubar = self.gui_manager.create_menubar()

        file_menu = self.menubar.add_menu("&File")
        file_menu.add_item("New...", editor.gui_level_new)
        file_menu.add_item("Open...", editor.gui_level_load)
        self.recent_files_menu = file_menu.add_menu("Open Recent")
        for filename in Config.current.recent_files:
            self.recent_files_menu.add_item(filename, lambda filename=filename: editor.load_level(filename))

        file_menu.add_item("Save...", editor.gui_level_save)
        # file_menu.add_item("Save Commands...", menu_file_save_commands)
        # file_menu.add_item("Save As...", editor.gui_level_save_as)
        file_menu.add_item("Properties...", editor.gui_edit_level)
        file_menu.add_item("Quit",  editor.gui.quit)

        edit_menu = self.menubar.add_menu("&Edit")
        edit_menu.add_item("Smooth Selection", editor.gui_smooth_level_struct)
        edit_menu.add_item("Resize", editor.gui_resize_sector)
        edit_menu.add_item("Resize to selection", editor.gui_resize_sector_to_selection)

        zoom_menu = self.menubar.add_menu("&Zoom")
        zoom_menu.add_item("1:4 (25%) ", lambda: editor.gui_set_zoom(0.25))
        zoom_menu.add_item("1:2 (50%) ", lambda: editor.gui_set_zoom(0.5))
        zoom_menu.add_item("1:1 (100%) ", lambda: editor.gui_set_zoom(1.0))
        zoom_menu.add_item("2:1 (200%) ", lambda: editor.gui_set_zoom(2.0))
        zoom_menu.add_item("4:1 (400%) ", lambda: editor.gui_set_zoom(4.0))

        layer_menu = self.menubar.add_menu("&Layer")
        layer_menu.add_item("Show all", editor.gui_show_all)
        layer_menu.add_item("Show current", editor.gui_show_current)
        layer_menu.add_item("Show only current", editor.gui_show_only_current)

        sector_menu = self.menubar.add_menu("&Sector")
        # sector = editor.workspace.get_map().metadata
        # for i in sector.parent.get_sectors():
        #     if sector.name == i:
        #         current = " [current]"
        #     else:
        #         current = ""
        #
        #     def on_sector_callback():
        #         print("Switching to %s" % i)
        #         editor.workspace.get_map().metadata.parent.activate_sector(i, editor.workspace)
        #
        #     mymenu.add_item(mysprite, ("Sector (%s)%s" % [i, current]), on_sector_callback)
        sector_menu.add_item("Create New Sector", editor.gui_add_sector)
        sector_menu.add_item("Remove Current Sector", editor.gui_remove_sector)
        sector_menu.add_item("Edit Sector Properties", editor.gui_edit_sector)


# EOF #
