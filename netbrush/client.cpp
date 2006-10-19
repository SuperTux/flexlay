#include <iostream>
#include <sstream>
#include <map>
#include "math/rect.hpp"
#include "stroke.hpp"
#include "SDL.h"
#include "SDL_main.h"
#include "SDL_image.h"
#include "SDL_net.h"
#include "drawing_context.hpp"
#include "drawing_parameter.hpp"
#include "debug.hpp"
#include "video.hpp"
#include "client_state.hpp"
#include "globals.hpp"
#include "screen_buffer.hpp"
#include "stroke_buffer.hpp"
#include "widget/widget_manager.hpp"
#include "widget/scrollbar.hpp"
#include "widget/button.hpp"
#include "saturation_value_picker.hpp"
#include "hue_picker.hpp"
#include "alpha_picker.hpp"
#include "brush_widget.hpp"
#include "navigation.hpp"
#include "server_connection.hpp"
#include "widget/slider_widget.hpp"

SDL_Rect* make_rect(int x, int y, int w, int h)
{
  static SDL_Rect rect;
  rect.x = x;
  rect.y = y;
  rect.w = w;
  rect.h = h;
  return &rect;
}

void process_events()
{
  SDL_Event event;
  
  while(SDL_PollEvent(&event))
    {
      switch(event.type)
        {
        case SDL_QUIT:
          puts("# quit");
          exit(0);
          break;

        case SDL_KEYDOWN:
          if (event.key.keysym.sym == SDLK_1)
            {
              client_draw_param->set_brush("brush_2x2.png");
            }
          else if (event.key.keysym.sym == SDLK_2)
            {
              client_draw_param->set_brush("brush_white_2x2.png");
            }
          else if (event.key.keysym.sym == SDLK_3)
            {
              client_draw_param->set_brush("brush_3x3.png");
            }
          else if (event.key.keysym.sym == SDLK_4)
            {
              client_draw_param->set_brush("brush_white_3x3.png");
            }
          else if (event.key.keysym.sym == SDLK_5)
            {
              client_draw_param->set_brush("brush_14x14.png");
            }
          else if (event.key.keysym.sym == SDLK_6)
            {
              client_draw_param->set_brush("brush_white_14x14.png");
            }
          else if (event.key.keysym.sym == SDLK_k)
            {
              std::cout << "Forced screen clear and update" << std::endl;
              SDL_FillRect(screen, NULL, SDL_MapRGB(screen->format, 255, 0, 255));
              SDL_UpdateRect(screen, 0, 0, 0, 0);
            }
          else if (event.key.keysym.sym == SDLK_j)
            {
              std::cout << "Forced screen update" << std::endl;
              SDL_UpdateRect(screen, 0, 0, 0, 0);
            }
          else if (event.key.keysym.sym == SDLK_c)
            {
              server->send("clear\n");
            }
          else if (event.key.keysym.sym == SDLK_INSERT)
            {
              client_draw_param->color.r = std::min(255, client_draw_param->color.r + 32);
            }
          else if (event.key.keysym.sym == SDLK_DELETE)
            {
              client_draw_param->color.r = std::max(0, client_draw_param->color.r - 32);
            }
          else if (event.key.keysym.sym == SDLK_HOME)
            {
              client_draw_param->color.g = std::min(255, client_draw_param->color.g + 32);
            }
          else if (event.key.keysym.sym == SDLK_END)
            {
              client_draw_param->color.g = std::max(0, client_draw_param->color.g - 32);
            }
          else if (event.key.keysym.sym == SDLK_PAGEUP)
            {
              client_draw_param->color.b = std::min(255, client_draw_param->color.b + 32);
            }
          else if (event.key.keysym.sym == SDLK_PAGEDOWN)
            {
              client_draw_param->color.b = std::max(0, client_draw_param->color.b - 32);
            }
          else if (event.key.keysym.sym == SDLK_UP)
            {
              client_draw_param->opacity = std::max(0, client_draw_param->opacity - 16);
            }
          else if (event.key.keysym.sym == SDLK_DOWN)
            {
              client_draw_param->opacity = std::min(255, client_draw_param->opacity + 16);
            }
          else if (event.key.keysym.sym == SDLK_u)
            {
              navigation->update();
            }
          break;

        case SDL_MOUSEBUTTONDOWN:
          widget_manager->on_mouse_button(event.button);
          break;

        case SDL_MOUSEBUTTONUP:
          widget_manager->on_mouse_button(event.button);
          break;

        case SDL_MOUSEMOTION:
          widget_manager->on_mouse_motion(event.motion);
          break;
        }
    }  
}

class RadiusCallback : public SliderCallback
{
public:
  void operator()(float v) 
  {
    float radius = v * 100.0f + 0.1f;
    client_draw_param->generic_brush.radius = radius;
    //std::cout << "Radius: " << radius << std::endl;
    brush_widget->set_brush(client_draw_param->generic_brush);
  }
};

class SpikeCallback : public SliderCallback
{
public:
  void operator()(float v) 
  {
    int spikes = int(v*18) + 2;
    //std::cout << "Spike: " << spikes << std::endl;
    client_draw_param->generic_brush.spikes = spikes;    
    brush_widget->set_brush(client_draw_param->generic_brush);
  }
};

class HardnessCallback : public SliderCallback
{
public:
  void operator()(float v) 
  {
    float hardness = v;
    client_draw_param->generic_brush.hardness = hardness;
    //std::cout << "Hardness: " << hardness << std::endl;
    brush_widget->set_brush(client_draw_param->generic_brush);
  }
};

class AspectRatioCallback : public SliderCallback
{
public:
  void operator()(float v) 
  {
    float aspect_ratio = v*19.0f + 1.0f;
    client_draw_param->generic_brush.aspect_ratio = aspect_ratio;
    //std::cout << "Aspect_Ratio: " << aspect_ratio << std::endl;
    brush_widget->set_brush(client_draw_param->generic_brush);
  }
};

class AngleCallback : public SliderCallback
{
public:
  void operator()(float v) 
  {
    float angle = v * 360.0f;
    client_draw_param->generic_brush.angle = angle;
    //std::cout << "Angle: " << angle << std::endl;
    brush_widget->set_brush(client_draw_param->generic_brush);
  }
};

class ToolButtonCallback : public ButtonCallback
{
private:
  DrawingParameter::Tool tool;
public:
  ToolButtonCallback(DrawingParameter::Tool tool_)
    : tool(tool_)
  {
  }

  void on_press  (Button* button) 
  {
    std::cout << "Press: " << button << std::endl;
  }

  void on_release(Button* button) 
  {
    std::cout << "Release: " << button << std::endl;
  }
  
  void on_click  (Button* button) 
  {
    std::cout << "Setting tool: " << tool << std::endl;
    client_draw_param->tool = tool;
  }
};

int main(int argc, char** argv)
{
  if(SDL_Init(SDL_INIT_VIDEO)== -1) {
    printf("SDL_Init: %s\n", SDL_GetError());
    exit(1);
  }
  atexit(SDL_Quit);

  if(SDLNet_Init()==-1) {
    printf("SDLNet_Init: %s\n", SDLNet_GetError());
    exit(2);
  }
  atexit(SDLNet_Quit);

  screen = SDL_SetVideoMode(1024, 768, 32, SDL_HWSURFACE); 
  if (screen == 0)
    printf("SDL_SetVideoMode: %s\n", SDL_GetError());
  SDL_WM_SetCaption("netBrush", "netBrush");

  // 18 is scrollbar
  screen_buffer = new ScreenBuffer(Rect(38, 2, screen->w - 128 - 16 - 18, screen->h - 16 - 4)); 
  draw_ctx      = new DrawingContext(2048, 2048);
  stroke_buffer = new StrokeBuffer(2048, 2048);

  std::cout << "# clear screen" << std::endl;

  // clear screen
  draw_ctx->clear();

  std::cout << "# clear screen done" << std::endl;

  client_draw_param = new DrawingParameter();
  stroke_buffer->set_param(client_draw_param);
  
  server = new ServerConnection();
  if (argc == 3)
    {
      std::cout << "# connecting to: " << argv[1] << ":" << atoi(argv[2]) << std::endl;
      server->connect(argv[1], atoi(argv[2]));
      std::ostringstream title_line;
      title_line << "netBrush - online: " << argv[1] << ":" << atoi(argv[2]);
      SDL_WM_SetCaption(title_line.str().c_str(), "netBrush");
    }
  else
    {
      std::cout << "# use '" << argv[0] << " HOSTNAME PORT' to connect a networking session" << std::endl;
      SDL_WM_SetCaption("netBrush - offline mode", "netBrush");
    }
  
  widget_manager = new WidgetManager();
  widget_manager->add(navigation = new Navigation(Rect(screen->w - 128, screen->h - 128, screen->w, screen->h)));
  widget_manager->add(new Button(IMG_Load("data/icons/stock-tool-airbrush-22.png"), 
                                 Rect(Point(2, 2+0*34), Size(34, 34)),
                                 new ToolButtonCallback(DrawingParameter::TOOL_AIRBRUSH)));
  widget_manager->add(new Button(IMG_Load("data/icons/stock-tool-paintbrush-22.png"), 
                                 Rect(Point(2, 2+1*34), Size(34, 34)),
                                 new ToolButtonCallback(DrawingParameter::TOOL_PAINTBRUSH)));
  if (0)
    widget_manager->add(new Button(IMG_Load("data/icons/stock-tool-zoom-22.png"), 
                                   Rect(Point(2, 2+2*34), Size(34, 34)),
                                   new ToolButtonCallback(DrawingParameter::TOOL_PAINTBRUSH)));

  {
    SDL_Rect color_rect;
    color_rect.x = 768;
    color_rect.y = 100;

    color_rect.w = 128;
    color_rect.h = 128;

    //widget_manager->add(new ColorSelector(&color_rect));
  }

  widget_manager->add(screen_buffer);

  widget_manager->add(vertical_scrollbar = 
                      new Scrollbar(0, 2048, screen_buffer->get_rect().get_height(), Scrollbar::VERTICAL,
                                    Rect(screen->w - 128 - 16 - 16, 2,
                                         screen->w - 128 - 16, screen->h - 16 - 4)));

  widget_manager->add(horizontal_scrollbar = 
                      new Scrollbar(0, 2048, screen_buffer->get_rect().get_width(), Scrollbar::HORIZONTAL,
                                    Rect(38, screen->h - 16 - 2,
                                         screen->w - 128 - 16 - 18, screen->h - 2)));

  alpha_picker = new AlphaPicker(Rect(Point(screen->w-128, 128+24), Size(128, 24)));
  saturation_value_picker = new SaturationValuePicker(Rect(Point(screen->w-128, 0), Size(128, 128)));
  hue_picker   = new HuePicker(Rect(Point(screen->w-128, 128), Size(128, 24)));

  brush_widget = new BrushWidget(Rect(Point(screen->w-128, 128+24+24), Size(128, 128)));
  brush_widget->set_brush(client_draw_param->generic_brush);

  SliderWidget* radius_slider = new SliderWidget(Rect(Point(screen->w-128, 128+24+24+128+24*(0)), Size(128, 24)),
                                                 new RadiusCallback());
  widget_manager->add(radius_slider);

  SliderWidget* spike_slider = new SliderWidget(Rect(Point(screen->w-128, 128+24+24+128+24*(1)), Size(128, 24)),
                                                 new SpikeCallback());
  widget_manager->add(spike_slider);

  SliderWidget* hardness_slider = new SliderWidget(Rect(Point(screen->w-128, 128+24+24+128+24*(2)), Size(128, 24)),
                                                 new HardnessCallback());
  widget_manager->add(hardness_slider);

  SliderWidget* aspect_ratio_slider = new SliderWidget(Rect(Point(screen->w-128, 128+24+24+128+24*(3)), Size(128, 24)),
                                                 new AspectRatioCallback());
  widget_manager->add(aspect_ratio_slider);

  SliderWidget* angle_slider = new SliderWidget(Rect(Point(screen->w-128, 128+24+24+128+24*(4)), Size(128, 24)),
                                                 new AngleCallback());
  widget_manager->add(angle_slider);

  widget_manager->add(saturation_value_picker);
  widget_manager->add(hue_picker);
  widget_manager->add(alpha_picker);
  widget_manager->add(brush_widget);

  // Main Loop
  while(true)
    {
      process_events();
      server->update();
      widget_manager->update();
      SDL_Delay(10);
    }
  
  return 0;
}

/* EOF */

