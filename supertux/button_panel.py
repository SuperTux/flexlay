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


class SuperTuxButtonPanel:

    def __init__(self, gui_manager, editor):
        # Create Buttonpanel
        self.button_panel = gui_manager.create_button_panel(True)

        # File Handling
        self.button_panel.add_icon("data/images/icons24/stock_new.png",  editor.gui_level_new)
        self.button_panel.add_icon("data/images/icons24/stock_open.png", editor.gui_level_load)

        self.button_panel.add_icon("data/images/icons24/stock_save.png", editor.gui_level_save)
        self.button_panel.add_icon("data/images/icons24/stock_save_as.png", editor.gui_level_save_as)

        # Copy&Paste
        self.button_panel.add_separator()
        self.button_panel.add_icon("data/images/icons24/stock_copy.png", None)
        self.button_panel.add_icon("data/images/icons24/stock_paste.png", None)
        # Undo Redo
        self.button_panel.add_separator()
        self.undo_icon = self.button_panel.add_icon("data/images/icons24/stock_undo.png", editor.undo)
        self.redo_icon = self.button_panel.add_icon("data/images/icons24/stock_redo.png", editor.redo)
        self.undo_icon.disable()
        self.redo_icon.disable()

        # Visibility Toggles
        self.button_panel.add_separator()
        self.minimap_icon = self.button_panel.add_icon("data/images/icons24/minimap.png", editor.gui_toggle_minimap)
        self.grid_icon = self.button_panel.add_icon("data/images/icons24/grid.png", editor.gui_toggle_grid)

        # Zoom Buttons
        self.button_panel.add_separator()
        self.button_panel.add_icon("data/images/icons24/stock_zoom_in.png", editor.gui_zoom_in)
        self.button_panel.add_icon("data/images/icons24/stock_zoom_out.png", editor.gui_zoom_out)
        self.button_panel.add_icon("data/images/icons24/stock_zoom_1.png", lambda: editor.gui_set_zoom(1.0))
        self.button_panel.add_icon("data/images/icons24/stock_zoom_fit.png", editor.gui_zoom_fit)

        # Raise/Lower
        self.button_panel.add_separator()
        self.button_panel.add_icon("data/images/icons24/object_lower_to_bottom.png", editor.lower_selection_to_bottom)
        self.button_panel.add_icon("data/images/icons24/object_lower.png", editor.lower_selection)
        self.button_panel.add_icon("data/images/icons24/object_raise.png", editor.raise_selection)
        self.button_panel.add_icon("data/images/icons24/object_raise_to_top.png", editor.raise_selection_to_top)

        # Layers
        self.button_panel.add_separator()
        self.background_icon = self.button_panel.add_icon("data/images/icons24/background.png",
                                                          editor.gui_show_background)
        self.interactive_icon = self.button_panel.add_icon("data/images/icons24/interactive.png",
                                                           editor.gui_show_interactive)
        self.foreground_icon = self.button_panel.add_icon("data/images/icons24/foreground.png",
                                                          editor.gui_show_foreground)
        self.button_panel.add_separator()
        self.run_icon = self.button_panel.add_icon("data/images/icons24/run.png", editor.gui_run_level)


# EOF #
