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


from typing import TYPE_CHECKING

from flexlay.gui.icon import Icon

if TYPE_CHECKING:
    from flexlay.gui_manager import GUIManager
    from supertux.gui import SuperTuxGUI


class SuperTuxToolbox:
    """A for selecting tools."""

    def __init__(self, gui_manager: 'GUIManager', editor: 'SuperTuxGUI') -> None:
        """
        :param gui_manager: a flexlay.GuiManager
        :param editor: a SuperTuxGui
        """
        # Create Toolbox
        self.toolbox = gui_manager.create_button_panel(False)
        self.toolbox.add_separator()
        self.object_icon = self.toolbox.add_icon(SuperTuxToolbox.icon_path("move"),
                                                 editor.set_objmap_select_tool, hover="Select Tool")

        self.paint_icon = self.toolbox.add_icon(SuperTuxToolbox.icon_path("tiles", 24),
                                                editor.set_tilemap_paint_tool, hover="Pencil Tool")
        self.fill_icon = self.toolbox.add_icon(SuperTuxToolbox.icon_path("fill", 24),
                                               editor.set_tilemap_fill_tool, hover="Fill Tool")
        self.replace_icon = self.toolbox.add_icon(SuperTuxToolbox.icon_path("replace", 24),
                                                  editor.set_tilemap_replace_tool, hover="Replace Tool")
        self.select_icon = self.toolbox.add_icon(SuperTuxToolbox.icon_path("rect-select"),
                                                 editor.set_tilemap_select_tool, hover="Rectangle Select Tool")

        self.toolbox.add_separator()
        self.zoom_icon = self.toolbox.add_icon(SuperTuxToolbox.icon_path("zoom"),
                                               editor.set_zoom_tool, hover="Zoom Tool")

        self.icons = [self.paint_icon, self.fill_icon, self.replace_icon,
                      self.select_icon, self.object_icon, self.zoom_icon]

    def set_down(self, rhs: Icon) -> None:
        for icon in self.icons:
            if icon == rhs:
                icon.set_down()
            else:
                icon.set_up()

    @staticmethod
    def icon_path(tool_name: str, size: int = 22) -> str:
        return "data/images/tools/stock-tool-%s-%s.png" % (tool_name, size)


# EOF #
