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

$layout_spec = [
  [:vbox,
    [:components,
      [:menubar,     
        [:name,   "menubar"], 
        [:height, 32],
        [:spec,   $menu_spec]],
      [:buttonpanel, 
        [:name,   "buttonpanel"],
        [:height, 32], 
        [:spec,   $buttonpanel_spec]],
      [:hbox, 
        [:components
          [:component, "editormap"],
          [:component, "selectorpanel"]]
      ],
    ]
  ]
]

# Class to handle automatic layouting of GUI components and resize
# events.
class LayoutManager
  def initialize(spec)
    @spec    
  end

  def set_size(width, height)
    
  end

  def add(name, component)

  end
end

## EOF ##
