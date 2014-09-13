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


class SpriteAction:

    def __init__(self):
        self.name = None
        self.image = None
        self.x_offset = None
        self.y_offset = None

    def parse(self, sexpr):
        self.name = get_value_from_tree(["name", "_"], sexpr, "default")
        self.x_offset = get_value_from_tree(["x-offset", "_"], sexpr, 0)
        self.y_offset = get_value_from_tree(["y-offset", "_"], sexpr, 0)
        # we only parse the first image for now as we don't have support for
        # animation in flexlay anyway
        self.image = get_value_from_tree(["images", "_"], sexpr, 0)


class Sprite:

    def __init__(self, filename):
        self.actions = {}

        tree = sexpr_read_from_file(filename)
        if tree is None:
            raise Exception("Error: Couldn't load: '%s'" % filename)

        self.basedir = File.dirname(filename) + "/"

        tree[1..-1].each do |i|
        case i[0]
        when :action
        action = SpriteAction()
        action.parse(i[1..-1])
        self.actions[action.name] = action
        if self.actions.default is None or action.name == "default":
            self.actions.default = action
        else:
            print "Unknown symbol '#{i[0]}' in sprite '#{filename}'"

    def get_cl_sprite(self, action="default"):
        action = self.actions[action]
        sprite = make_sprite(self.basedir + action.image)
        # FIXME:
        # sprite.set_frame_offset(0, Point(action.x_offset, action.y_offset))
        return sprite


# EOF #
