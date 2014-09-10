from PyQt5.Core import Qt
from PyQt5.Gui import QScrollArea
from flexlay.gui import ObjectSelectorWidget


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
