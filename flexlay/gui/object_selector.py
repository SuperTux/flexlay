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


from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QScrollArea
from .object_selector_widget import ObjectSelectorWidget


class ObjectSelector:

    def __init__(self, obj_w, obj_h, parent):
        self.scroll_area = QScrollArea(parent)
        self.scroll_area.setWidgetResizable(True)
        self.scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self.scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        self.widget = ObjectSelectorWidget(obj_w, obj_h, self.scroll_area.viewport())
        self.scroll_area.setWidget(self.widget)

    def add_brush(self, brush):
        self.widget.add_brush(brush)

    def sig_drop(self):
        return self.widget.sig_drop()

    def get_widget(self):
        return self.scroll_area


# EOF #
