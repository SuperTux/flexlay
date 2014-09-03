//  Flexlay - A Generic 2D Game Editor
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

#include <iostream>
#include <ClanLib/Display/keys.h>
#include <ClanLib/GUI/component.h>

#include "colorpicker.hpp"
#include "math.hpp"
#include "math/rect.hpp"

class ColorPickerHue : public CL_Component
{
public:
  std::vector<CL_Slot> slots;
  typedef std::vector<CL_Color> Colors;
  Colors  colors;
  bool pressed;
  boost::signals2::signal<void (CL_Color)> on_color_change;

  ColorPickerHue(const Rect& rect, CL_Component* parent) :
    CL_Component(rect.to_cl(), parent),
    pressed(false)
  {
    colors.push_back(CL_Color(255,   0,   0));
    colors.push_back(CL_Color(255,   0, 255));
    colors.push_back(CL_Color(  0,   0, 255));
    colors.push_back(CL_Color(  0, 255, 255));
    colors.push_back(CL_Color(  0, 255,   0));
    colors.push_back(CL_Color(255, 255,   0));
    colors.push_back(CL_Color(255,   0,   0));

    slots.push_back(sig_paint().connect(this, &ColorPickerHue::draw));

    slots.push_back(sig_mouse_down().connect(this, &ColorPickerHue::on_mouse_down));
    slots.push_back(sig_mouse_up().connect(this, &ColorPickerHue::on_mouse_up));
    slots.push_back(sig_mouse_move().connect(this, &ColorPickerHue::on_mouse_move));
  }

  void update_pointer(const CL_InputEvent& event)
  {
    CL_Color new_color;

    if (event.mouse_pos.y >= get_height() || event.mouse_pos.y < 0)
    {
      new_color = colors[0];
    }
    else
    {
      float factor  = (float(event.mouse_pos.y) / get_height()) * (colors.size()-1);
      int   prevcol = int(factor);
      int   nextcol = prevcol+1;

      float val  = factor - prevcol;
      float ival = 1.0f - val;


      if (val >= 0 && val < 1.0f)
      {
        new_color = CL_Color(int(val * colors[nextcol].get_red()   + ival * colors[prevcol].get_red()),
                             int(val * colors[nextcol].get_green() + ival * colors[prevcol].get_green()),
                             int(val * colors[nextcol].get_blue()  + ival * colors[prevcol].get_blue()),
                             int(val * colors[nextcol].get_alpha() + ival * colors[prevcol].get_alpha()));
      }
      else
      {
        std::cout << "Out of range" << std::endl;
        new_color = colors[0];
      }
    }
    on_color_change(new_color);

    /*
      std::cout << new_color.get_red() << ", "
      << new_color.get_green() << ", "
      << new_color.get_blue() << ", "
      << new_color.get_alpha()
      << std::endl;*/
  }

  void on_mouse_up(const CL_InputEvent& event)
  {
    if (event.id == CL_MOUSE_LEFT)
    {
      pressed = false;
      release_mouse();
      update_pointer(event);
    }
  }

  void on_mouse_down(const CL_InputEvent& event)
  {
    if (event.id == CL_MOUSE_LEFT)
    {
      pressed = true;
      capture_mouse();
      update_pointer(event);
    }
  }

  void on_mouse_move(const CL_InputEvent& event)
  {
    if (pressed)
    {
      update_pointer(event);
    }
  }

  void draw()
  {
    CL_Display::push_modelview();
    CL_Display::add_translate(get_screen_x(), get_screen_y());

    int psize = get_height()/6;

    for(Colors::size_type i = 0; i < colors.size()-1; ++i)
    {
      CL_Display::fill_rect(Rect(Point(0, i*psize),
                                 Size(get_width(), psize)).to_cl(),
                            CL_Gradient(colors[i],
                                        colors[i],
                                        colors[i+1],
                                        colors[i+1]));
    }

    CL_Display::pop_modelview();
  }
};

class ColorPickerAlpha : public CL_Component
{
public:
  std::vector<CL_Slot> slots;
  bool pressed;
  boost::signals2::signal<void (float)> on_color_change;
  float alpha;

  ColorPickerAlpha(const Rect& rect, CL_Component* parent) :
    CL_Component(rect.to_cl(), parent),
    pressed(false),
    alpha(0.5f)
  {
    slots.push_back(sig_paint().connect(this, &ColorPickerAlpha::draw));

    slots.push_back(sig_mouse_down().connect(this, &ColorPickerAlpha::on_mouse_down));
    slots.push_back(sig_mouse_up().connect(this, &ColorPickerAlpha::on_mouse_up));
    slots.push_back(sig_mouse_move().connect(this, &ColorPickerAlpha::on_mouse_move));
  }

  void set_alpha(float alpha_)
  {
    alpha = alpha_;
    on_color_change(alpha);
  }

  void draw()
  {
    CL_Display::push_modelview();
    CL_Display::add_translate(get_screen_x(), get_screen_y());

    CL_Display::fill_rect(Rect(Point(0, 0),
                               Size(get_width(), get_height())).to_cl(),
                          CL_Gradient(CL_Color(0, 0, 0),
                                      CL_Color(255, 255, 255),
                                      CL_Color(0, 0, 0),
                                      CL_Color(255, 255, 255)));

    CL_Display::pop_modelview();
  }

  void update_pointer(const CL_InputEvent& event)
  {
    alpha = 1.0f - (Math::mid(0.0f, float(event.mouse_pos.x) / get_width(), 1.0f));
    on_color_change(alpha);
  }

  void on_mouse_up(const CL_InputEvent& event)
  {
    if (event.id == CL_MOUSE_LEFT)
    {
      pressed = false;
      release_mouse();
      update_pointer(event);
    }
  }

  void on_mouse_down(const CL_InputEvent& event)
  {
    if (event.id == CL_MOUSE_LEFT)
    {
      pressed = true;
      capture_mouse();
      update_pointer(event);
    }
  }

  void on_mouse_move(const CL_InputEvent& event)
  {
    if (pressed)
    {
      update_pointer(event);
    }
  }
};

class ColorPickerBrightness : public CL_Component
{
public:
  std::vector<CL_Slot> slots;
  CL_Color color;
  bool pressed;
  boost::signals2::signal<void (CL_Color)> on_color_change;
  float factor_x;
  float factor_y;
  ColorPickerBrightness(const Rect& rect, CL_Component* parent) :
    CL_Component(rect.to_cl(), parent),
      pressed(false),
      factor_x(1.0f),
      factor_y(1.0f)
  {
    color = CL_Color(255, 0, 0);
    slots.push_back(sig_paint().connect(this, &ColorPickerBrightness::draw));

    slots.push_back(sig_mouse_down().connect(this, &ColorPickerBrightness::on_mouse_down));
    slots.push_back(sig_mouse_up().connect(this, &ColorPickerBrightness::on_mouse_up));
    slots.push_back(sig_mouse_move().connect(this, &ColorPickerBrightness::on_mouse_move));
  }

  void draw()
  {
    CL_Display::push_modelview();
    CL_Display::add_translate(get_screen_x(), get_screen_y());

    CL_Display::fill_rect(Rect(Point(0, 0), Size(get_width(), get_height())).to_cl(),
                          CL_Gradient(CL_Color(0, 0, 0),
                                      color,
                                      CL_Color(0, 0, 0),
                                      CL_Color(255, 255, 255)));

    CL_Display::draw_line(factor_x * get_width(),
                          0,
                          factor_x * get_width(),
                          get_height(),
                          CL_Color(255, 255, 255));

    CL_Display::draw_line(0,
                          factor_y * get_height(),
                          get_width(),
                          factor_y * get_height(),
                          CL_Color(255, 255, 255));

    CL_Display::pop_modelview();
  }

  void set_color(CL_Color color_) {
    color = color_;
    update_color();
  }

  void update_color()
  {
    CL_Color new_color(Math::mid(0, int(factor_x * color.get_red()   * (1.0f - factor_y) + factor_x * 255 * (factor_y)), 255),
                       Math::mid(0, int(factor_x * color.get_green() * (1.0f - factor_y) + factor_x * 255 * (factor_y)), 255),
                       Math::mid(0, int(factor_x * color.get_blue()  * (1.0f - factor_y) + factor_x * 255 * (factor_y)), 255),
                       color.get_alpha());
    on_color_change(new_color);
    /*
      std::cout << new_color.get_red() << ", "
      << new_color.get_green() << ", "
      << new_color.get_blue() << ", "
      << new_color.get_alpha()
      << std::endl;
    */
  }

  void update_pointer(const CL_InputEvent& event)
  {
    factor_x = Math::mid(0.0f, float(event.mouse_pos.x)/get_width(), 1.0f);
    factor_y = Math::mid(0.0f, float(event.mouse_pos.y)/get_height(), 1.0f);

    update_color();
  }

  void on_mouse_up(const CL_InputEvent& event)
  {
    if (event.id == CL_MOUSE_LEFT)
    {
      pressed = false;
      release_mouse();
      update_pointer(event);
    }
  }

  void on_mouse_down(const CL_InputEvent& event)
  {
    if (event.id == CL_MOUSE_LEFT)
    {
      pressed = true;
      capture_mouse();
      update_pointer(event);
    }
  }

  void on_mouse_move(const CL_InputEvent& event)
  {
    if (pressed)
    {
      update_pointer(event);
    }
  }
};

ColorPicker::ColorPicker(const Rect& rect, CL_Component* parent) :
  CL_Component(rect.to_cl(), parent)
{
  float pwidth  = rect.get_width()/11.0;
  float pheight = rect.get_height()/11.0;

  brightness = new ColorPickerBrightness(Rect(Point(0, 0),
                                                 Size(int(pwidth*10), int(pheight*10))),
                                         this);

  hue = new ColorPickerHue(Rect(Point(int(pwidth*10), 0),
                                          Size(int(pwidth*1), int(pheight*10))),
                                  this);

  alpha = new ColorPickerAlpha(Rect(Point(0, int(pheight*10)),
                                            Size(int(pwidth*10), int(pheight*1))),
                                    this);

  hue->on_color_change.connect([this](const CL_Color& c){ set_color(c); });
  brightness->on_color_change.connect([this](const CL_Color& c){ update_brightness_color(c); });
  alpha->on_color_change.connect([this](float v){ update_alpha_color(v); });

  slots.push_back(sig_paint().connect(this, &ColorPicker::draw));

  brightness->set_color(CL_Color(255, 0, 0));
  alpha->set_alpha(0.5f);
}

void
ColorPicker::update_alpha_color(float alpha)
{
  color.set_alpha(int(255 * alpha));
  on_color_change(color);
}

void
ColorPicker::update_brightness_color(CL_Color color_)
{
  color.set_red(color_.get_red());
  color.set_green(color_.get_green());
  color.set_blue(color_.get_blue());
  on_color_change(color);
}

void
ColorPicker::draw()
{
  CL_Display::push_modelview();
  CL_Display::add_translate(get_screen_x(), get_screen_y());

  float pwidth  = get_width()/11.0;
  float pheight = get_height()/11.0;
  CL_Display::fill_rect(Rect(Point(int(pwidth*10), int(pheight*10)),
                             Size(int(pwidth), int(pheight))).to_cl(),
                        color);

  CL_Display::pop_modelview();
}

  boost::signals2::signal<void (CL_Color)>&
ColorPicker::sig_color_change()
{
  return on_color_change;
}

CL_Color
ColorPicker::get_color()
{
  return color;
}

void
ColorPicker::set_color(const CL_Color& color_)
{
  color = color_;
}

/* EOF */
