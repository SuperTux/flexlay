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


from typing import Any, Optional

from flexlay import Colorf
from flexlay.util import get_value_from_tree
from flexlay.gui.generic_dialog import GenericDialog
from flexlay.util.sexpr_writer import SExprWriter


class Property:
    """
    A property is just that: a property
    What these classes do is allow properties to easily be written to files,
    and displayed in dialogs.
    @see: flexlay/gui/generic_dialog.py, supertux/properties_widget.py
    """

    # Editable means appears in GenericDialog
    editable = False

    def __init__(self, label: str, identifier: str, default: Any, optional: bool = False) -> None:
        self.label: str = label
        self.identifier: str = identifier
        self.value: Any = default
        self.default: Any = default
        self.optional: bool = optional

    def read(self, sexpr: Any, obj: Any) -> None:
        self.value = get_value_from_tree([self.identifier, "_"], sexpr, self.default)

    def write(self, writer: SExprWriter, obj: Any) -> None:
        if not self.optional or self.value != self.default:
            writer.write(self.identifier, self.value)

    def property_dialog(self, dialog: GenericDialog) -> None:
        pass

    def on_value_change(self, value: Any) -> None:
        self.value = value


class BoolProperty(Property):

    editable = True

    def property_dialog(self, dialog: GenericDialog) -> None:
        dialog.add_bool(self.label, self.value, self.on_value_change)


class IntProperty(Property):

    editable = True

    def __init__(self, label: str, identifier: str, default: int = 0, optional: bool = False) -> None:
        super().__init__(label, identifier, default, optional)

    def property_dialog(self, dialog: GenericDialog) -> None:
        dialog.add_int(self.label, self.value, callback=self.on_value_change)


class FloatProperty(Property):

    editable = True

    def __init__(self, label: str, identifier: str, default: float = 0.0, optional: bool = False) -> None:
        super().__init__(label, identifier, default, optional)

    def property_dialog(self, dialog: GenericDialog) -> None:
        dialog.add_float(self.label, self.value, callback=self.on_value_change)


class StringProperty(Property):

    editable = True
    placeholder: Optional[str] = None

    def __init__(self, label: str, identifier: str,
                 default: str = "",
                 optional: bool = False,
                 translatable: bool = False,
                 placeholder: Optional[str] = None) -> None:
        super().__init__(label, identifier, default, optional)
        self.translatable = translatable

        if placeholder is not None:
            self.placeholder = placeholder

    def read(self, sexpr: Any, obj: Any) -> None:
        self.value = get_value_from_tree([self.identifier, "_"], sexpr, self.default)

    def write(self, writer: SExprWriter, obj: Any) -> None:
        if not self.optional or self.value != self.default:
            writer.write_string(self.identifier, self.value, translatable=self.translatable)

    def property_dialog(self, dialog: GenericDialog) -> None:
        dialog.add_string(self.label, self.value, self.on_value_change, self.placeholder)
        # dialog.add_string(self.label, self.value, self.on_value_change)


class FileProperty(StringProperty):

    editable = True

    def __init__(self, label: str, identifier: str, default: str = "",
                 relative_to: str = "", open_in: str = "") -> None:
        """
        :param relative_to: The prefix text not displayed in the input box
        :param open_in: Where the browse dialog opens
        """
        super().__init__(label, identifier, default=default)
        # Where the file dialog opens by default
        self.open_in = open_in

        # Where the path shown is relative to (if possible)
        self.relative_to = relative_to

        # The actual path stored, so that the relative path can be displayed.
        self.actual_path = ""

    def property_dialog(self, dialog: GenericDialog) -> None:
        dialog.add_file(self.label, self.default, self.relative_to, self.open_in, self.on_value_change)


class EnumProperty(StringProperty):

    editable = True

    def __init__(self,
                 label: str,
                 identifier: str,
                 default: int,
                 optional: bool = False,
                 values: Optional[list[Any]] = list[Any]) -> None:
        """
        :param default: Is an index from values!!!
        """
        super().__init__(label, identifier, values[default] if values else "", optional=optional)

        self.default_index = default

        if values is None:
            values = []

        self.values = values

    def property_dialog(self, dialog: GenericDialog) -> None:
        dialog.add_enum(self.label, self.values, self.values.index(self.value), self.on_value_change)


class ColorProperty(Property):

    editable = True

    def __init__(self,
                 label: str,
                 identifier: str,
                 default: Colorf = Colorf(1.0, 1.0, 1.0),
                 optional: bool = False) -> None:
        super().__init__(label, identifier, default=default, optional=optional)

    def read(self, sexpr: Any, obj: Any) -> None:
        self.value = Colorf(*get_value_from_tree([self.identifier], sexpr, [1.0, 1.0, 1.0]))

    def write(self, writer: SExprWriter, obj: Any) -> None:
        if not self.optional or self.value != self.default:
            if self.value.a == 1.0:
                writer.write_color(self.identifier, self.value.to_list()[0:3])
            else:
                writer.write_color(self.identifier, self.value.to_list())

    def property_dialog(self, dialog: GenericDialog) -> None:
        dialog.add_color(self.label, self.value)


# EOF #
