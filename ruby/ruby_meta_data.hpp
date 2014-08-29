//  $Id$
// 
//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_RUBY_META_DATA_HXX
#define HEADER_RUBY_META_DATA_HXX

#include <ClanLib/signals.h>
#include <ClanLib/Display/color.h>
#include "ruby.h"
#include "../lib/meta_data.hpp"
#include "../lib/objmap_object.hpp"
#include "../lib/object_brush.hpp"

MetaData  make_metadata(VALUE obj);
VALUE get_ruby_object(const MetaData& data);

void connect(CL_Signal_v0& sig, VALUE obj);
void connect_v1(CL_Signal_v1<int>& sig, VALUE obj);
void connect_v1_float(CL_Signal_v1<float>& sig, VALUE obj);
void connect_v2(CL_Signal_v2<int, int>& sig, VALUE obj);
void connect_v2_graceful(CL_Signal_v2<int, int>& sig, VALUE obj);

void connect_v1_Color(CL_Signal_v1<CL_Color>& sig, VALUE func);
void connect_v1_ObjMapObject(CL_Signal_v1<ObjMapObject>& sig, VALUE func);
void connect_v2_ObjectBrush_Point(CL_Signal_v2<ObjectBrush, CL_Point>& sig, VALUE func);

#endif

/* EOF */
