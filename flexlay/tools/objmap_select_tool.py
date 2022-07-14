# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
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


from typing import Optional, TYPE_CHECKING

from PyQt5.QtGui import QIcon, QCursor
from PyQt5.QtWidgets import QMenu

from flexlay.color import Color
from flexlay.input_event import InputEvent
from flexlay.workspace import Workspace
from flexlay.tool_context import ToolContext
from flexlay.commands.object_move_command import ObjectMoveCommand
from flexlay.commands.object_delete_command import ObjectDeleteCommand
from flexlay.gui.editor_map_component import EditorMapComponent
from flexlay.math import Pointf, Rectf
from flexlay.tools.tool import Tool
from flexlay.util.signal import Signal
from flexlay.objmap_object import ObjMapObject
from flexlay.objmap_control_point import ObjMapControlPoint
from flexlay.graphic_context import GraphicContext


if TYPE_CHECKING:
    from flexlay.gui_manager import GUIManager


class ObjMapSelectTool(Tool):

    STATE_NONE = 0
    STATE_DRAG = 1
    STATE_SELECT = 2

    def __init__(self, gui_manager: 'GUIManager') -> None:
        assert ToolContext.current is not None

        super().__init__()

        self.manager = gui_manager

        self.state = ObjMapSelectTool.STATE_NONE
        # Left click + drag rectangle
        self.drag_start = Pointf(0, 0)
        self.selection_rect = Rectf(0, 0, 0, 0)

        # For selected objects do: self.context.object_selection
        self.deselected: list[ObjMapObject] = []  # Objects that were selected before

        self.offset = Pointf(0, 0)
        self.move_command: Optional[ObjectMoveCommand] = None

        self.control_point: Optional[ObjMapControlPoint] = None
        self.context: ToolContext = ToolContext.current

        # Never used:
        # self.sig_popup_menu_display = Signal()
        self.sig_right_click = Signal()

    def clear_selection(self) -> None:
        assert self.context is not None
        self.context.object_selection.clear()
        self.on_selection_change()

    def get_selection(self) -> list[ObjMapObject]:
        assert self.context is not None
        return self.context.object_selection

    def set_selection(self, selection: list[ObjMapObject]) -> None:
        assert self.context is not None
        self.context.object_selection = selection

    def draw(self, gc: GraphicContext) -> None:
        assert self.context is not None

        self.deselected = self.context.object_selection

        for obj in self.context.object_selection:
            bound_rect = obj.get_bound_rect()

            if bound_rect is not None:
                gc.draw_rect(bound_rect, Color(0, 0, 192), 3)
                gc.fill_rect(bound_rect, Color(0, 0, 128, 64), 3)

        if self.state == ObjMapSelectTool.STATE_DRAG:
            pass
        elif self.state == ObjMapSelectTool.STATE_SELECT:
            gc.draw_rect(self.selection_rect, Color(255, 255, 255))

    def on_mouse_up(self, event: InputEvent) -> None:
        # print("ObjMapSelectToolImpl.on_mouse_up ", event.kind, event.mouse_pos.x, event.mouse_pos.y)
        assert EditorMapComponent.current is not None
        assert ToolContext.current is not None
        assert event.mouse_pos is not None

        objmap = ToolContext.current.object_layer
        parent = EditorMapComponent.current
        pos = parent.screen2world(event.mouse_pos.to_f())

        if event.kind == InputEvent.MOUSE_LEFT:
            if self.state == ObjMapSelectTool.STATE_DRAG:
                if self.move_command:
                    assert Workspace.current is not None
                    Workspace.current.get_map().execute(self.move_command)
                    self.move_command = None
                self.state = ObjMapSelectTool.STATE_NONE
                parent.release_mouse()

            elif self.state == ObjMapSelectTool.STATE_SELECT:
                self.state = ObjMapSelectTool.STATE_NONE

                self.selection_rect.right = pos.x
                self.selection_rect.bottom = pos.y
                self.selection_rect.normalize()

                assert self.context is not None
                assert objmap is not None
                self.context.object_selection = objmap.get_selection(self.selection_rect)

                self.on_selection_change()
                parent.release_mouse()

        elif event.kind == InputEvent.MOUSE_RIGHT:
            # GRUMBEL sig_right_click(event.mouse_pos.x + parent.get_screen_rect().left,
            #               event.mouse_pos.y + parent.get_screen_rect().top)
            pass

    def on_mouse_down(self, event: InputEvent) -> None:
        # print("ObjMapSelectToolImpl.on_mouse_down ", event.kind, event.mouse_pos.x, event.mouse_pos.y)
        assert EditorMapComponent.current is not None
        assert ToolContext.current is not None
        assert event.mouse_pos is not None
        assert self.context is not None
        assert Workspace.current is not None

        objmap = ToolContext.current.object_layer
        assert objmap is not None
        parent = EditorMapComponent.current
        pos = parent.screen2world(event.mouse_pos.to_f())

        if event.kind == InputEvent.MOUSE_LEFT:
            self.control_point = objmap.find_control_point(pos)
            if self.control_point:
                self.state = ObjMapSelectTool.STATE_DRAG
                parent.grab_mouse()
                self.offset = pos - self.control_point.get_pos()
                self.drag_start = pos
            else:
                obj = objmap.find_object(pos)
                if obj is not None:
                    if event.mod & InputEvent.MOD_SHIFT:
                        # print("ObjMapSelectTool: Shift presses")
                        if obj not in self.context.object_selection:
                            self.context.object_selection.append(obj)
                        else:
                            self.context.object_selection.remove(obj)

                        self.on_selection_change()
                    else:
                        self.state = ObjMapSelectTool.STATE_DRAG
                        parent.grab_mouse()
                        self.offset = pos - obj.get_pos()
                        self.drag_start = pos

                        if obj not in self.context.object_selection:
                            # Clicked object is not in the selection, so we add it
                            self.context.object_selection.clear()
                            objmap.delete_control_points()
                            self.context.object_selection.append(obj)
                            self.on_selection_change()

                        self.move_command = ObjectMoveCommand(objmap)
                        for obj in self.context.object_selection:
                            self.move_command.add_obj(obj)
                else:
                    self.state = ObjMapSelectTool.STATE_SELECT
                    self.selection_rect = Rectf(pos.x, pos.y, pos.x, pos.y)
                    parent.grab_mouse()

        elif event.kind == InputEvent.MOUSE_RIGHT:
            self.sig_right_click()
            obj = objmap.find_object(pos)
            menu = QMenu()
            # Is there an object under cursor?
            if len(self.context.object_selection) > 0 and obj:
                # Add object actions to menu
                def delete_obj() -> None:
                    assert self.context.object_layer is not None
                    delete_command = ObjectDeleteCommand(self.context.object_layer)
                    delete_command.objects = self.context.object_selection
                    self.context.object_selection = []
                    assert Workspace.current is not None
                    Workspace.current.get_map().execute(delete_command)

                def show_obj_properties() -> None:
                    for i in self.get_selection():
                        i.metadata.property_dialog()

                delete_action = menu.addAction(QIcon("data/images/icons24/stock_delete.png"), "Delete Object(s)")
                delete_action.triggered.connect(delete_obj)
                properties_action = menu.addAction(QIcon("data/images/icons24/stock_edit.png"), "View Properties")
                properties_action.triggered.connect(show_obj_properties)
                menu.addSeparator()
            menu.move(QCursor.pos())
            menu.exec_()

    def on_mouse_move(self, event: InputEvent) -> None:
        # print("ObjMapSelectToolImpl.on_mouse_move ", event.kind, event.mouse_pos.x, event.mouse_pos.y)
        assert EditorMapComponent.current is not None
        assert ToolContext.current is not None
        assert event.mouse_pos is not None
        assert self.context is not None

        parent = EditorMapComponent.current
        pos = parent.screen2world(event.mouse_pos.to_f())

        if self.state == ObjMapSelectTool.STATE_DRAG:
            if self.control_point:
                self.control_point.set_pos(pos - self.offset)
            else:
                assert self.move_command is not None
                self.move_command.move_by(pos - self.drag_start)

                if len(self.context.object_selection) == 1:
                    self.context.object_selection[0].update_control_points()

        elif self.state == ObjMapSelectTool.STATE_SELECT:
            self.selection_rect.right = pos.x
            self.selection_rect.bottom = pos.y

    def on_selection_change(self) -> None:
        assert self.context is not None

        for obj in self.deselected:
            obj.sig_deselect(self.manager)
        if len(self.context.object_selection) != 1:
            for obj in self.context.object_selection:
                obj.sig_select(None)
        else:
            self.context.object_selection[0].sig_select(self.manager)
        # selected = self.context.object_selection

        assert ToolContext.current is not None
        assert ToolContext.current.object_layer is not None
        objmap = ToolContext.current.object_layer
        objmap.delete_control_points()

        if len(self.context.object_selection) == 1:
            self.context.object_selection[0].add_control_points()


# EOF #
