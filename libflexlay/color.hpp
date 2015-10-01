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

//! clanDisplay="Display 2D"
//! header=display.h

#ifndef header_flexlay_color
#define header_flexlay_color

#include <QColor>

#include <string>
#include <vector>

//: Color description class.
//- !group=Display/Display 2D!
//- !header=display.h!
class Color
{
//! Construction:
public:
	//: Constructs a color.
	//- <p>Color components are specified in the range 0 to 255.</p>
	//- <p>An alpha value of 0 means complete transparency, while 255 means completely opaque (solid).</p>
	//param red: Red color component.
	//param green: Green color component.
	//param blue: Blue color component.
	//param alpha: Alpha (transparency) color component.
	Color() : color(0) { return; }

  Color(unsigned int red, unsigned int green, unsigned int blue, unsigned int alpha = 255) :
        color((alpha<<24) | (red<<16) | (green<<8) | blue) { return; }

QColor to_qt() const
  {
    return QColor(get_red(), get_green(), get_blue(), get_alpha());
  }

//! Attributes:
public:
	//: Returns the alpha color component, in the range 0-255.
	unsigned int get_alpha() const { return (color >> 24) & 0xff; }

	//: Returns the red color component, in the range 0-255.
	unsigned int get_red() const { return (color >> 16) & 0xff; }

	//: Returns the green color component, in the range 0-255.
	unsigned int get_green() const { return (color >> 8) & 0xff; }

	//: Returns the blue color component, in the range 0-255.
	unsigned int get_blue() const { return color & 0xff; }

	//: Color in ARGB8888 format.
	unsigned int color;

// Operations:
public:
	//: Color == Color operator (deep compare)
	bool operator==(const Color &c) const
	{ return (color == c.color); }

#ifndef SWIG
	//: Color != Color operator (deep compare)
	bool operator!=(const Color &c) const
	{ return (color != c.color); }
#endif

//! Operations:
public:
	//: Set alpha color component, in the range 0-255.
	void set_alpha(unsigned int value) { color = (color & 0x00ffffff) | (value << 24); }

	//: Set red color component, in the range 0-255.
	void set_red(unsigned int value) { color = (color & 0xff00ffff) | (value << 16); }

	//: Set green color component, in the range 0-255.
	void set_green(unsigned int value) { color = (color & 0xffff00ff) | (value << 8); }

	//: Set blue color component, in the range 0-255.
	void set_blue(unsigned int value) { color = (color & 0xffffff00) | value; }

	//: Set color based on rgba color components in the range 0-255.
	void set_color(unsigned int red, unsigned int green, unsigned int blue, unsigned int alpha = 255)
	{ color = (alpha<<24) | (red<<16) | (green<<8) | blue; }
};

#endif
