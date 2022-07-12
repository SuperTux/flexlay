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


from typing import Any, IO, Optional

import os

from flexlay.util import get_value_from_tree
from flexlay.util.sexpr_writer import SExprWriter

from supertux.util import load_lisp
from supertux.level import Level


class Addon:

    @staticmethod
    def from_file(filename: str) -> 'Addon':
        addon = Addon()

        # if filename.endswith(".stwm"):
        #    level.is_worldmap = True
        # else:
        #    level.is_worldmap = False

        # level.filename = filename

        assert addon.filename is not None
        tree: Any = load_lisp(addon.filename, "supertux-addoninfo")
        data = tree[1:]

        addon.id = get_value_from_tree(["id", "_"], data, "")
        addon.version = get_value_from_tree(["version", "_"], data, "")
        addon.type = get_value_from_tree(["type", "_"], data, "")
        addon.title = get_value_from_tree(["title", "_"], data, "")
        addon.author = get_value_from_tree(["author", "_"], data, "")
        addon.license = get_value_from_tree(["license", "_"], data, "")

        # print("VERSION:", level.filename, " ", level.version)

        # if level.version == 1:
        #    raise Exception("version 1 levels not supported at the moment")
        # else:
        #    level.parse_v2(data)

        return addon

    def __init__(self) -> None:
        self.id = "no-id"
        self.version: int = 1
        self.type: str = "levelset"
        self.title: str = "No Title"
        self.author: str = "No Author"
        self.license: str = "GPL 2+ / CC-by-sa 3.0"
        self.filename: Optional[str] = None

        # Arrays containing data that we can use in a SuperTux level:
        self.images: list[str] = []
        self.levels: list[Level] = []
        self.music: list[str] = []
        self.scripts: list[str] = []
        self.sounds: list[str] = []

        # Directory path of add-on:
        self.project_dir = None

    def parse_v2(self, data) -> None:
        self.id = get_value_from_tree(["id", "_"], data, "")
        self.version = get_value_from_tree(["version", "_"], data, "")
        self.type = get_value_from_tree(["type", "_"], data, "")
        self.title = get_value_from_tree(["title", "_"], data, "")
        self.author = get_value_from_tree(["author", "_"], data, "")
        self.license = get_value_from_tree(["license", "_"], data, "")

    def save(self, dirname: str) -> None:
        # Save project dir
        self.project_dir = dirname

        # Create directory, if it doesn't exist:
        if not os.path.exists(dirname):
            os.makedirs(dirname)
        with open(os.path.join(dirname, self.id + ".nfo"), "w") as f:
            self.save_io(f)

        # Create directory structure:
        self.subdirs = ["images",
                        os.path.join("levels", self.id),
                        "music",
                        "scripts",
                        "sounds"]

        for subdir in self.subdirs:
            path = os.path.join(dirname, subdir)
            if not os.path.exists(path):
                os.makedirs(path)

        # Save SuperTux sub-world info file:
        info_file_path = os.path.join(dirname, "levels", self.id, "info")
        with open(info_file_path, "w") as f:
            self.save_info_file(f)

        # TODO: Save levels and other things (assets) here:
        print("TODO: Save levels in add-on directory")
        for level in self.levels:
            print(level.name)

        for image in self.images:
            pass  # TODO: Save images

        for music_file in self.music:
            pass  # TODO: Save music

        for script in self.scripts:
            pass  # TODO: Save scripts

        for sound in self.sounds:
            pass  # TODO: Save sounds

    def save_io(self, f: IO[str]) -> None:
        writer = SExprWriter(f)
        writer.write_comment("Generated by Flexlay Level Editor (Development Version)")
        writer.begin_list("supertux-addoninfo")
        writer.write_string("id", self.id)
        writer.write_int("version", self.version)
        writer.write_string("type", self.type)
        writer.write_string("title", self.title)
        writer.write_string("author", self.author)
        if self.license:
            writer.write_string("license", self.license)
        writer.end_list()

    def save_info_file(self, f: IO[str]) -> None:
        writer = SExprWriter(f)
        writer.write_comment("Generated by Flexlay Level Editor (Development Version)")
        writer.begin_list("supertux-world")
        writer.write_string("title", self.title)
        writer.write_string("description", "Example add-on")
        writer.write_bool("hide-from-contribs", False)
        writer.write_bool("levelset", False)
        writer.end_list()

    def get_project_dir(self) -> str:
        return self.project_dir

    def set_project_dir(self, project_dir) -> None:
        self.project_dir = project_dir


# EOF #
