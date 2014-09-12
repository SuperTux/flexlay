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


from PyQt5.QtWidgets import QWidget, QGridLayout, QScrollBar
from PyQt5.QtCore import Qt

from ..workspace import Workspace
from flexlay.math import Pointf

from .editor_map_widget import EditorMapWidget


class EditorMapComponent:

    def __init__(self, parent):
        self.editormap_widget = None
        self.workspace = Workspace()

        self.widget = QWidget(parent)
        self.layout = QGridLayout(self.widget)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.setHorizontalSpacing(0)
        self.layout.setVerticalSpacing(0)

        self.scroll_horz = QScrollBar(Qt.Horizontal)
        self.scroll_vert = QScrollBar(Qt.Vertical)
        self.editormap_widget = EditorMapWidget(self)

        self.scroll_horz.valueChanged.connect(lambda value: self.move_to_x(value))
        self.scroll_vert.valueChanged.connect(lambda value: self.move_to_y(value))

        self.layout.addWidget(self.editormap_widget, 0, 0)
        self.layout.addWidget(self.scroll_horz, 1, 0)
        self.layout.addWidget(self.scroll_vert, 0, 1)

    def get_workspace(self):
        return self.workspace

    def capture_mouse(self):
        self.editormap_widget.grabMouse()

    def release_mouse(self):
        self.editormap_widget.releaseMouse()

    # ifdef GRUMBEL
    # void
    # EditorMapComponentImpl::on_key_down(const CL_InputEvent& event)
    # {
    #   if (event.id >= 0 && event.id < 256)
    #   {
    #     Rect rect = parent.get_position()
    #     key_bindings[event.id](CL_Mouse::get_x() - rect.left,
    #                            CL_Mouse::get_y() - rect.top)
    #   }

    #   if (event.repeat_count == 0)
    #   {
    #     Rect rect = parent.get_position()
    #     CL_InputEvent ev2 = event
    #     ev2.mouse_pos = Point(CL_Mouse::get_x() - rect.left,
    #                           CL_Mouse::get_y() - rect.top).to_cl()
    #     workspace.key_down(InputEvent(ev2))
    #   }
    # }

    # void
    # EditorMapComponentImpl::on_key_up(const CL_InputEvent& event)
    # {
    #   Rect rect = parent.get_position()
    #   CL_InputEvent ev2 = event
    #   ev2.mouse_pos = Point(CL_Mouse::get_x() - rect.left,
    #                         CL_Mouse::get_y() - rect.top).to_cl()
    #   workspace.key_up(InputEvent(ev2))
    # }

    # void
    # EditorMapComponentImpl::draw ()
    # {
    #   if (workspace.get_map().is_null()) return

    #   Display::push_cliprect(parent.get_screen_rect())

    #   Display::push_modelview()
    #   Display::add_translate(parent.get_screen_x(), parent.get_screen_y())

    #   // Update scrollbars (FIXME: move me to function)
    #   scrollbar_v.set_range(0, workspace.get_map().get_bounding_rect().get_height())
    #   scrollbar_v.set_pagesize(parent.get_height()/gc_state.get_zoom())
    #   scrollbar_v.set_pos(gc_state.get_pos().y)

    #   scrollbar_h.set_range(0, workspace.get_map().get_bounding_rect().get_width())
    #   scrollbar_h.set_pagesize(parent.get_width()/gc_state.get_zoom())
    #   scrollbar_h.set_pos(gc_state.get_pos().x)

    #   gc_state.push()
    #   {
    #     GraphicContext gc(gc_state, CL_Display::get_current_window().get_gc())
    #     workspace.draw(gc)
    #   }
    #   gc_state.pop()

    #   Display::pop_modelview()
    #   Display::pop_cliprect()
    # }
    # endif

    def screen2world(self, pos):
        return self.gc_state.screen2world(pos)

    def set_zoom(self, z):
        self.gc_state.set_zoom(z)

        self.editormap_widget.repaint()
        self.update_scrollbars()

    def zoom_out(self, pos):
        self.gc_state.set_zoom(self.gc_state.get_zoom() / 1.25,
                               Pointf(pos.x, pos.y))

        self.editormap_widget.repaint()
        self.update_scrollbars()

    def zoom_in(self, pos):
        self.gc_state.set_zoom(self.gc_state.get_zoom() * 1.25,
                               Pointf(pos.x, pos.y))

        self.editormap_widget.repaint()
        self.update_scrollbars()

    def zoom_to(self, rect):
        self.gc_state.zooself.to(rect)

        self.editormap_widget.repaint()
        self.update_scrollbars()

    def get_clip_rect(self):
        return self.gc_state.get_clip_rect()

    def move_to(self, x, y):
        self.gc_state.set_pos(Pointf(x, y))

        self.editormap_widget.repaint()
        self.update_scrollbars()

    def move_to_x(self, x):

        self.gc_state.set_pos(Pointf(x, self.gc_state.get_pos().y))

        self.editormap_widget.repaint()
        self.update_scrollbars()

    def move_to_y(self, y):
        self.gc_state.set_pos(Pointf(self.gc_state.get_pos().x, y))

        self.editormap_widget.repaint()
        self.update_scrollbars()

    # ifdef GRUMBEL
    # void
    # EditorMapComponentImpl::on_resize(int old_w, int old_h)
    # {
    #   Rect rect = parent.get_screen_rect()

    #   gc_state.set_size(rect.get_width(), rect.get_height())
    # }
    # endif

    # ifdef GRUMBEL
    # boost::signals2::signal<void (int, int)>&
    # def sig_on_key(const std::string& str)
    # {
    #   int id = CL_Keyboard::get_device().string_to_keyid(str)

    #   //std::cout << str << " => " << id << std::endl

    #   if (id > 0 && id < 256)
    #   {
    #     return impl.key_bindings[id]
    #   }
    #   else
    #   {
    #     std::cout << "def sig_on_key: invalid key id: " << id << std::endl
    #     return impl.key_bindings[0]
    #   }
    # }
    # endif

    def get_gc_state(self):
        return self.gc_state

    def get_widget(self):
        return self.widget

    def update_scrollbars(self):
        rect = self.workspace.get_map().get_bounding_rect()
        border = 128

        self.scroll_horz.setMinimum(rect.left - border)
        self.scroll_horz.setMaximum(rect.right + border)
        self.scroll_horz.setPageStep(self.editormap_widget.width())
        self.scroll_horz.setSliderPosition(int(self.gc_state.get_pos().x))

        self.scroll_vert.setMinimum(rect.top - border)
        self.scroll_vert.setMaximum(rect.bottom + border)
        self.scroll_vert.setPageStep(self.editormap_widget.height())
        self.scroll_vert.setSliderPosition(int(self.gc_state.get_pos().y))


# EOF #
