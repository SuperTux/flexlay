//  $Id$
// 
//  Pingus - A free Lemmings clone
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

#ifndef HEADER_HELPER_HXX
#define HEADER_HELPER_HXX

#include <ClanLib/Display/sprite.h>
#include <ClanLib/Display/pixel_buffer.h>

CL_Sprite      pixelbuffer2sprite(const CL_PixelBuffer& buffer);
CL_Sprite      make_sprite(const std::string& filename);
CL_PixelBuffer make_pixelbuffer(const std::string& filename);
CL_PixelBuffer make_region_pixelbuffer(const std::string& filename, int x, int y, int w, int h);
CL_PixelBuffer make_pixelbuffer(int width, int height);

CL_Sprite      make_sprite_from_resource(const std::string& filename, CL_ResourceManager& resources);
CL_PixelBuffer make_pixelbuffer_from_resource(const std::string& filename, CL_ResourceManager& resources);
#endif

/* EOF */
