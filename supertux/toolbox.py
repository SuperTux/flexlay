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


class SuperTuxToolbox:

    def __init__(self, gui_manager, editor):
        # Create Toolbox
        self.toolbox = gui_manager.create_button_panel(False)
        self.paint_icon = self.toolbox.add_icon("data/images/tools/stock-tool-pencil-22.png",
                                                editor.set_tilemap_paint_tool)
        self.fill_icon = self.toolbox.add_icon("data/images/tools/stock-tool-fill-24.png",
                                               editor.set_tilemap_fill_tool)
        self.replace_icon = self.toolbox.add_icon("data/images/tools/stock-tool-replace-24.png",
                                                  editor.set_tilemap_replace_tool)
        self.select_icon = self.toolbox.add_icon("data/images/tools/stock-tool-rect-select-22.png",
                                                 editor.set_tilemap_select_tool)
        self.toolbox.add_separator()
        self.object_icon = self.toolbox.add_icon("data/images/tools/stock-tool-clone-22.png",
                                                 editor.set_objmap_select_tool)
        self.toolbox.add_separator()
        self.zoom_icon = self.toolbox.add_icon("data/images/tools/stock-tool-zoom-22.png",
                                               editor.set_zoom_tool)

        self.icons = [self.paint_icon, self.fill_icon, self.replace_icon,
                      self.select_icon, self.object_icon, self.zoom_icon]

    def set_down(self, rhs):
        for icon in self.icons:
            if icon == rhs:
                icon.set_down()
            else:
                icon.set_up()


# EOF #
