//  $Id: EditorTileMapView.cxx,v 1.2 2002/09/01 00:05:33 grumbel Exp $
//
//  Pingus - A free Lemmings clone
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#include <ClanLib/gl.h>
#include "EditorTileMap.hxx"
#include "EditorTileMapView.hxx"

EditorTileMapView::EditorTileMapView (EditorTileMap* m)
  : tilemap (m), scroll_state (SCROLL_OFF)
{
  tiles.push_back("tiles/green1");
  tiles.push_back("tiles/tile1");
  tiles.push_back("tiles/tile2");
  tiles.push_back("tiles/tile3");
  tiles.push_back("tiles/tile4");
  tiles.push_back("tiles/tile5");
  tiles.push_back("tiles/tile6");
  tiles.push_back("tiles/tile7");
  tiles.push_back("tiles/tile8");
  tiles.push_back("tiles/tile9");
  tiles.push_back("tiles/tile10");
  tiles.push_back("tiles/tile11");
  tiles.push_back("tiles/tile12");
  tiles.push_back("tiles/tile13");
  tiles.push_back("tiles/tile14");
  tiles.push_back("tiles/tile15");
  tiles.push_back("tiles/tile16");
  tiles.push_back("tiles/tile17");
  tiles.push_back("tiles/tile18");
  tiles.push_back("tiles/tile19");
  tiles.push_back("tiles/tile20");
  tiles.push_back("tiles/tile21");
  tiles.push_back("tiles/tile22");
  tiles.push_back("tiles/tile23");
  tiles.push_back("tiles/tile24");
  tiles.push_back("tiles/tile25");
  tiles.push_back("tiles/tile26");
  tiles.push_back("tiles/tile27");
  tiles.push_back("tiles/tile29");
  tiles.push_back("tiles/tile30");
  tiles.push_back("tiles/tile31");
  tiles.push_back("tiles/tile32");
  tiles.push_back("tiles/tile33");
  tiles.push_back("tiles/tile34");
  tiles.push_back("tiles/tile35");
  tiles.push_back("tiles/tile36");
  tiles.push_back("tiles/tile39");
  tiles.push_back("tiles/tile40");
  tiles.push_back("tiles/tile41");
  tiles.push_back("tiles/tile42");
  tiles.push_back("tiles/tile43");
  tiles.push_back("tiles/tile44");
  tiles.push_back("tiles/tile45");
  tiles.push_back("tiles/tile46");
  tiles.push_back("tiles/tile47");
  tiles.push_back("tiles/tile48");
  tiles.push_back("tiles/tile49");
  tiles.push_back("tiles/tile50");
  tiles.push_back("tiles/tile51");
  tiles.push_back("tiles/tile52");
  tiles.push_back("tiles/tile53");
  tiles.push_back("tiles/tile54");
  tiles.push_back("tiles/tile55");
  tiles.push_back("tiles/tile56");
  tiles.push_back("tiles/tile57");
  tiles.push_back("tiles/tile58");
  tiles.push_back("tiles/tile59");
  tiles.push_back("tiles/tile60");
  tiles.push_back("tiles/tile61");
  tiles.push_back("tiles/tile62");
  tiles.push_back("tiles/tile63");
  tiles.push_back("tiles/tile64");
  tiles.push_back("tiles/tile65");
  tiles.push_back("tiles/tile66");
  tiles.push_back("tiles/tile67");
  tiles.push_back("tiles/tile68");
  tiles.push_back("tiles/tile69");

  tiles.push_back("tiles/tile71");
  tiles.push_back("tiles/tile72");
  tiles.push_back("tiles/tile73");
  tiles.push_back("tiles/tile75");
  tiles.push_back("tiles/tile76");
  tiles.push_back("tiles/tile77");

  current_tile = tiles.begin ();
}

void
EditorTileMapView::draw ()
{
  glPushMatrix ();
  glTranslatef (-pos.x + CL_Display::get_width ()/2,
		-pos.y + CL_Display::get_height ()/2, 0.0);
  tilemap->draw ();

#if 0
  if (CL_Mouse::left_pressed())
  {
    CL_Vector mouse (CL_Mouse::get_x () - CL_Display::get_width ()/2, 
		     CL_Mouse::get_y () - CL_Display::get_height ()/2);
    mouse += pos;

    int tile_x = int(mouse.x)/64;
    int tile_y = int(mouse.y)/64;

    std::cout << "Tile: " << tile_x << ", " << tile_y << std::endl;
    EditorTile* tile = tilemap->get_tile (tile_x, tile_y);

    if (tile) {
      tile->set_tile (*current_tile);
    }
  }


  if (CL_Mouse::middle_pressed())
  {
    CL_Vector mouse (CL_Mouse::get_x () - CL_Display::get_width ()/2, 
		     CL_Mouse::get_y () - CL_Display::get_height ()/2);
    mouse += pos;

    int tile_x = int(mouse.x)/64;
    int tile_y = int(mouse.y)/64;

    std::cout << "Tile: " << tile_x << ", " << tile_y << std::endl;
    EditorTile* tile = tilemap->get_tile (tile_x, tile_y);

    if (tile)
      tile->set_tile ("none");
  }

  if (CL_Keyboard::get_keycode (CL_KEY_UP))
    {
      ++current_tile;
	
      if (current_tile == tiles.end ())
	current_tile = tiles.begin ();

      current_brush_tile.set_tile (*current_tile);
      CL_System::sleep (100);
    }
  else if (CL_Keyboard::get_keycode (CL_KEY_DOWN))
    {
      if (current_tile == tiles.begin ())
	{
	  current_tile = tiles.end ();
	  --current_tile;
	}
      else
	{
	  --current_tile;
	}
      current_brush_tile.set_tile (*current_tile);
      CL_System::sleep (100);
    }
#endif
  glPopMatrix ();

  current_brush_tile.draw (0, 0);

}

void
EditorTileMapView::update ()
{
#if 0
  if (CL_Mouse::right_pressed ())
    {
      if (scroll_state == SCROLL_OFF)
	{
	  scroll_state = SCROLL_ON;
	  scroll_pos = CL_Vector (CL_Mouse::get_x (), CL_Mouse::get_y ());
	}
      else 
	{
	  pos += (CL_Vector (CL_Mouse::get_x (), CL_Mouse::get_y ()) - scroll_pos) * 0.1;
	}
    }
  else 
    {
      scroll_state = SCROLL_OFF;
    }

  if (CL_Keyboard::get_keycode (CL_KEY_F5))
    {
      std::string filename;
      std::cout << "Input filename to save: " << std::flush;
      std::cin >> filename;
      filename = std::string("../data/levels/") + filename;
      tilemap->save (filename);
      std::cout << "Saving to: " << filename << std::endl;
    }
#endif 0
}

/* EOF */
