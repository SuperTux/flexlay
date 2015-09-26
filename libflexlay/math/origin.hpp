/*
**  ClanLib SDK
**  Copyright (c) 1997-2005 The ClanLib Team
**
**  This software is provided 'as-is', without any express or implied
**  warranty.  In no event will the authors be held liable for any damages
**  arising from the use of this software.
**
**  Permission is granted to anyone to use this software for any purpose,
**  including commercial applications, and to alter it and redistribute it
**  freely, subject to the following restrictions:
**
**  1. The origin of this software must not be misrepresented; you must not
**     claim that you wrote the original software. If you use this software
**     in a product, an acknowledgment in the product documentation would be
**     appreciated but is not required.
**  2. Altered source versions must be plainly marked as such, and must not be
**     misrepresented as being the original software.
**  3. This notice may not be removed or altered from any source distribution.
**
**  Note: Some of the libraries ClanLib may link to may have additional
**  requirements or restrictions.
**
**  File Author(s):
**
**    Magnus Norddahl
**    (if your name is missing here, please add it)
*/

//! clanCore="Math"
//! header=core.h

#ifndef header_flexlay_origin
#define header_flexlay_origin

#include "size.hpp"
#include "point.hpp"

//: Alignment origins.
//- !group=Core/Math!
//- !header=core.h!
enum Origin
{
	Flexlay_origin_top_left,
	Flexlay_origin_top_center,
	Flexlay_origin_top_right,
	Flexlay_origin_center_left,
	Flexlay_origin_center,
	Flexlay_origin_center_right,
	Flexlay_origin_bottom_left,
	Flexlay_origin_bottom_center,
	Flexlay_origin_bottom_right
};

//: Returns the anchor point for the origin within the dimensions of the size structure.
//- !group=Display/Display 2D!
//- !header=display.h!
Point calc_origin(Origin origin, const Size &size);
Pointf calc_origin(Origin origin, const Sizef &size);

#endif
