from PyQt5.GUI import QSizePolicy


class ObjectSelectorWidget:

    def __init__(cell_w, cell_h, viewport, parent):
        super(parent)

        self.viewport = viewport
        self.cell_width = cell_w
        self.cell_height = cell_h
        self.brushes = None,
        self.has_focus = false

        self.index = 0

        self.mouse_over_tile = -1
        self.scrolling = false
        self.scale = 1.0
        self.drag_obj = -1

        self.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Expanding)
        self.setMouseTracking(True)

    def minimumSizeHint():
        columns = get_columns()
        min_rows = (self.brushes.size() + columns - 1) / columns
        return QSize(self.cell_width * get_columns(),
                     self.cell_height * min_rows)

    def resizeEvent(event):
        pass

    def get_columns():
        return self.viewport.width() / self.cell_width

    def mousePressEvent(event):
        if event.button() == Qt.LeftButton:
            if mouse_over_tile != -1:
                drag_obj = mouse_over_tile

                if (drag_obj != -1):
                    drag = QDrag(self)
                    mimeData = QMimeData()
                    object = SuperTuxBadGuyData()
                    #GRUMBEL QByteArray data(reinterpret_cast<const char*>(&object), sizeof(object))
                    mimeData.setData("application/supertux-badguy", data)
                    drag.setMimeData(mimeData)

                    pixmap = QPixmap.fromImage(self.brushes[drag_obj].get_sprite().get_pixelbuffer().get_qimage())
                    drag.setPixmap(pixmap)
                    drag.setHotSpot(QPoint(self.brushes[drag_obj].get_sprite().get_width()/2,
                                           self.brushes[drag_obj].get_sprite().get_height()/2))

                    print("Starting drag")
                    result = drag.exec()
                    print("Starting drag finished")

                    self.drag_obj = -1

        elif event.button() == Qt.MidButton:
            self.scrolling = true
            self.click_pos = Point(event.pos())
            self.old_offset = offset
            # GRUMBEL: ui.scrollArea.horizontalScrollBar().setValue(100)
            self.releaseMouse()

    def mouseReleaseEvent(event):
        if event.button() == Qt.LeftButton:
            if drag_obj != -1:
                # releaseMouse()
                self.drag_obj = -1

        elif event.button() == Qt.MidButton:
            self.scrolling = false
            releaseMouse()

    def mouseMoveEvent(event):
        if self.scrolling:
            self.offset = self.old_offset + (self.click_pos.y - event.y())

        self.mouse_pos = Point(event.pos())

        cell_w = width() / get_columns()
        x = event.x() // cell_w
        y = (event.y() + offset) // self.cell_height

        self.mouse_over_tile = y * get_columns() + x

        if self.mouse_over_tile < 0 or self.mouse_over_tile >= len(self.brushes):
            self.mouse_over_tile = -1

        self.repaint()

    def wheelEvent(event):
        numDegrees = event.delta() / 8
        numSteps = numDegrees / 15

        self.offset += int(self.cell_height * scale * numSteps)

    def paintEvent(event):
      if self.offset < 0:
          self.offset = 0

      painter = QPainter(self)
      gc = GraphicContext(painter)

      for i in range(len(self.brushes)):
        x = i % get_columns()
        y = i // get_columns()

        cell_w = width() / get_columns()
        rect = Rectf(x * cell_w, y * self.cell_height,
                     (x+1) * cell_w, (y+1) * self.cell_height)

        if (x + y - 1) % 2 == 0:
          gc.fill_rect(rect, Color(224, 224, 224))
        else:
          gc.fill_rect(rect, Color(192, 192, 192))

        sprite = self.brushes[i].get_sprite()
        sprite.set_alignment(Flexlay_origin_center, 0, 0)
        sprite.set_scale(min(1.0f, self.cell_width / sprite.get_width()),
                         min(1.0f, self.cell_height / sprite.get_height()))
        sprite.draw(rect.left + rect.get_width() / 2,
                    rect.top + rect.get_height() / 2,
                    gc)

        # highlight the current selection
        if mouse_over_tile == i and self.has_focus:
          gc.fill_rect(rect, Color(0, 0, 255, 20))

    def enterEvent(event):
      self.has_focus = true

    def leaveEvent(event):
      self.has_focus = false
      self.repaint()

    def add_brush(brush):
        self.brushes.push_back(brush)

    def sig_drop():
        return on_drop

# EOF #
