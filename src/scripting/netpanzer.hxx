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

#ifndef HEADER_SCRIPTING_NETPANZER_HXX
#define HEADER_SCRIPTING_NETPANZER_HXX

#include <string>

class EditorMap;
class EditorMapLayer;
struct NetPanzerFileStruct;

#ifndef SWIG
struct NetPanzerFileStruct
{
  std::string id_header;
  std::string name;
  std::string description;

  EditorMapLayer* tilemap; 
};
#endif

void netpanzer_set_id_header(NetPanzerFileStruct* npm, std::string s);
void netpanzer_set_name(NetPanzerFileStruct* npm, std::string s);
void netpanzer_set_description(NetPanzerFileStruct* npm, std::string s);
void netpanzer_set_tilemap(NetPanzerFileStruct* npm, EditorMapLayer* t);

std::string netpanzer_get_id_header(NetPanzerFileStruct* npm);
std::string netpanzer_get_name(NetPanzerFileStruct* npm);
std::string netpanzer_get_description(NetPanzerFileStruct* npm);
EditorMapLayer* netpanzer_get_tilemap(NetPanzerFileStruct* npm);

NetPanzerFileStruct* load_netpanzer_map(const char* filename);
void save_netpanzer_map(const char* filename, 
                        EditorMap* m, 
                        const char* id_header_, const char* name_, const char* description_);

#endif

/* EOF */
