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

#include <stdio.h>
#include <iostream>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/display_window.h>
#include <ClanLib/Display/pixel_buffer.h>
#include <ClanLib/Display/pixel_format.h>
#include "screenshot.hxx"

void
Screenshot::write_screenshot_pnm(const std::string& filename)
{
  CL_PixelBuffer* buf = take_screen_shot();

  FILE* out = fopen(filename.c_str(), "wb");
  
  if (!out)
    {
      perror(filename.c_str());
      std::cout << "Screenshot: Couldn't write file: " << filename << std::endl;
      return;
    }

  buf->lock();
  int width  = buf->get_width();
  int pitch  = buf->get_width()*3;
  int height = buf->get_height();

  fprintf(out,
	  "P6\n"
	  "# CREATOR: Feuerkraft\n"
          "%d %d\n"
	  "255\n",
	  width,
	  height);

  unsigned char* data = static_cast<unsigned char*>(buf->get_data());
  
  for(int i = height-1; i >= 0; --i)
    {
      fwrite(data + pitch*i,
             sizeof(unsigned char),
             pitch, 
             out);
    }

  buf->unlock();
  fclose(out);
  
  delete buf;
}

CL_PixelBuffer*
Screenshot::take_screen_shot()
{
  CL_PixelBuffer back_buffer = CL_Display::get_current_window()->get_back_buffer();
  
  unsigned short width = back_buffer.get_width();
  unsigned short height = back_buffer.get_height();
		
  CL_PixelBuffer *pbuf = new CL_PixelBuffer(width, height, width*3, CL_PixelFormat::bgr888);
  back_buffer.convert(pbuf);
  
  return pbuf;
}


/* EOF */
