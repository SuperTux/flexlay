##  $Id$
##   ______ __               ___
##  |   ___|  |.-----.--.--.|   | .---.-.--.--.
##  |   ___|  ||  -__|_   _||   |_|  _  |  |  |
##  |__|   |__||_____|__.__||_____|___._|___  |
##                                      |_____|
##  Copyright (C) 2004 Ingo Ruhnke <grumbel@gmx.de>
##
##  This program is free software; you can redistribute it and/or
##  modify it under the terms of the GNU General Public License
##  as published by the Free Software Foundation; either version 2
##  of the License, or (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
## 
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
##  02111-1307, USA.

$gameobjects = [
  ["streetlamp", "images/streetlamp.png", "sprite",
    proc{|data, sexpr| nil}],
]

class UnknownGameObject
  def initialize(name, sexpr, obj)
    @name  = name
    @sexpr = sexpr
    @obj   = obj
  end

  def save(f)
    f.write("    (#{@name}\n")
    @sexpr.each{|i|
      f.write("      ")
      write_sexpr(f, i)
      f.write("\n")
    }
    f.write("    )")
    f.puts
  end
end

## EOF ##
