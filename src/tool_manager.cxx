//  $Id$
//
//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#include <iostream>
#include "tilemap_paint_tool.hxx"
#include "tilemap_select_tool.hxx"
#include "objmap_select_tool.hxx"
#include "zoom_tool.hxx"
#include "editor_map.hxx"
#include "editor_names.hxx"
#include "tool_manager.hxx"

ToolManager::ToolManager()
{
  // FIXME: move this to scripting too
  tools.push_back(TileMapPaintTool().to_tool());
  tools.push_back(TileMapSelectTool().to_tool());
  //tools.push_back(Tool()); //new TileMapDiamondTool());
  tools.push_back(ObjMapSelectTool().to_tool());
  tools.push_back(ZoomTool().to_tool());

  //tool = tools[0]; 
  tool = 0;
}

ToolManager::~ToolManager()
{
}

void
ToolManager::set_tool(int i)
{
  if (i >= 0 && i < int(tools.size()))
    {
      if (tool != i)
        {
          on_tool_change();
          tool = i;
        }
    }
  else
    {
      std::cout << "Only have " << tools.size() << " tools, tool " << i << " can't be selected." << std::endl;
    }
}

CL_Signal_v0&
ToolManager::sig_tool_change()
{
  return on_tool_change;
}

Tool
ToolManager::current_tool()
{
  return tools[tool]; 
}

/* EOF */
