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


from flexlay.wip.bitmap_layer import BitmapLayer
from flexlay.blend_func import BlendFunc
from flexlay import DrawerProperties
from flexlay.math import Rect, Point, Size, Origin


class Surface:

    pass


class SpriteStrokeDrawer:

    def __init__(self, drawer) -> None:
        self.mode = SpriteStrokeDrawer.DM_NORMAL
        self.drawer = drawer

    def draw(self, stroke, gc):
        if DrawerProperties.current.get_brush().is_null() or stroke.get_dab_count() == 0:
            return

        dabs = stroke.get_interpolated_dabs(DrawerProperties.current.get_spacing() *
                                            DrawerProperties.current.get_size(),
                                            DrawerProperties.current.get_spacing() *
                                            DrawerProperties.current.get_size())

        for i, dab in enumerate(self.dabs):
            sprite = DrawerProperties.current.get_brush().get_sprite()
            color = DrawerProperties.current.get_color()

            sprite.set_color(color)
            sprite.set_alpha((color.get_alpha() / 255.0) * dab.pressure)
            sprite.set_scale(DrawerProperties.current.get_size() * dab.pressure,
                             DrawerProperties.current.get_size() * dab.pressure)

            if gc is not None:
                # DO Multipass:
                # 1: GL_ZERO, GL_DST_ALPHA
                # 2: GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA

                # brush.set_blend_func_separate(BlendFunc.zero, BlendFunc.dst_alpha,
                #                               BlendFunc.zero, BlendFunc.one)
                # brush.draw(dab.pos.x, dab.pos.y, gc)

                if self.mode == SpriteStrokeDrawer.DM_NORMAL:
                    sprite.set_blend_func_separate(BlendFunc.src_alpha, BlendFunc.one_minus_src_alpha,
                                                   BlendFunc.one, BlendFunc.one_minus_src_alpha)
                    sprite.draw(dab.pos.x, dab.pos.y, gc.gc)

                elif self.mode == SpriteStrokeDrawer.DM_ADDITION:
                    sprite.set_blend_func_separate(BlendFunc.src_alpha, BlendFunc.one,
                                                   BlendFunc.zero, BlendFunc.one)
                    # BlendFunc.one, BlendFunc.one_minus_src_alpha)
                    sprite.draw(dab.pos.x, dab.pos.y, gc.gc)

                elif self.mode == SpriteStrokeDrawer.DM_ERASE:
                    sprite.set_blend_func(BlendFunc.zero, BlendFunc.one_minus_src_alpha)
                    sprite.draw(dab.pos.x, dab.pos.y, gc.gc)

                elif self.mode == SpriteStrokeDrawer.DM_SMUDGE:
                    if dab != dabs[0]:
                        canvas = BitmapLayer.current.get_canvas()
                        buffer = canvas.get_pixeldata(
                            Rect(Point(int(self.dabs[i - 1].pos.x) - sprite.width / 2,
                                       int(self.dabs[i - 1].pos.y) - sprite.height / 2),
                                 Size(sprite.width, sprite.height)))
                        surface = Surface(buffer)
                        # surface.set_blend_func_separate(BlendFunc.src_alpha, BlendFunc.one_minus_src_alpha,
                        #                                BlendFunc.one, BlendFunc.zero)
                        surface.set_alignment(Origin.center)
                        surface.set_alpha(0.5)
                        # surface.set_scale(DrawerProperties.current.get_size(),
                        #                 DrawerProperties.current.get_size())
                        surface.draw(dab.pos.x, dab.pos.y, gc.gc)
                else:
                    print("Error: SpriteStrokeDrawer: Unknown draw mode: ", self.mode)
            else:
                if self.mode == SpriteStrokeDrawer.DM_NORMAL:
                    sprite.set_blend_func(BlendFunc.src_alpha, BlendFunc.one_minus_src_alpha)
                    sprite.draw(dab.pos.x, dab.pos.y, gc.gc)

                elif self.mode == SpriteStrokeDrawer.DM_ADDITION:
                    sprite.set_blend_func(BlendFunc.src_alpha, BlendFunc.one)
                    sprite.draw(dab.pos.x, dab.pos.y, gc.gc)

                elif self.mode == SpriteStrokeDrawer.DM_ERASE:
                    sprite.set_blend_func(BlendFunc.zero, BlendFunc.one_minus_src_alpha)
                    sprite.draw(dab.pos.x, dab.pos.y, gc.gc)

                elif self.mode == SpriteStrokeDrawer.DM_SMUDGE:
                    sprite.set_blend_func(BlendFunc.src_alpha, BlendFunc.one_minus_src_alpha)
                    sprite.draw(dab.pos.x, dab.pos.y, gc.gc)

                else:
                    print("Error: SpriteStrokeDrawer: Unknown draw mode:", self.mode)

    def set_mode(self, mode):
        self.mode = mode

    def get_mode(self):
        return self.mode


# EOF #
