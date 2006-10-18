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
#include "widget/button.hpp"
#include "saturation_value_picker.hpp"
#include "hue_picker.hpp"
#include "alpha_picker.hpp"
#include "brush_widget.hpp"
#include "widget/callback.hpp"
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

void connect(const char* hostname, Uint16 port)
{
  IPaddress ip;

  if(SDLNet_ResolveHost(&ip, hostname, port) == -1) 
    {
      printf("SDLNet_ResolveHost: %s\n", SDLNet_GetError());
      exit(1);
    }

  tcpsock = SDLNet_TCP_Open(&ip);
  if(!tcpsock)
    {
      printf("SDLNet_TCP_Open: %s %s:%d\n", SDLNet_GetError(), hostname, port);
      exit(2);
    }
  else
    {
      std::string line = "client_version 1\n";
      SDLNet_TCP_Send(tcpsock, const_cast<char*>(line.c_str()), line.length());

      socketset = SDLNet_AllocSocketSet(1);
      SDLNet_TCP_AddSocket(socketset, tcpsock);
    }
}


std::vector<std::string>
tokenize(const std::string& str, char split_char)
{
  std::string::size_type start = 0;
  std::string::size_type end   = 0;

  std::vector<std::string> tokens;

  while (start < str.size())
    {
      if ((end = str.find(split_char, start)) == std::string::npos)
        {
          tokens.push_back(str.substr(start));
          break;
        }

      const std::string& ret = str.substr(start, end - start);

      if (!ret.empty())
        tokens.push_back(ret);

      start = end + 1;
    }

  return tokens;
}

void process_command(const std::string& cmd)
{
  if (cmd.empty()) return;

  const std::vector<std::string>& tokens = tokenize(cmd, ' ');
  if (0)
    for(int i = 0; i < int(tokens.size()); ++i)
      std::cout << "Token: '" << tokens[i] << "'" << std::endl;

  if (!tokens.empty())
    {
      if (tokens[0] == "#")
        {
          // comment, ignore
        }
      else if (tokens[0] == "clear")
        {
          draw_ctx->clear();
        }
      else if (tokens[0] == "client")
        {
          if (tokens.size() > 2)
            {
              int client_id = atoi(tokens[1].c_str());
              // convert to stroke
              std::map<int, ClientState*>::iterator i = client_states.find(client_id);
              ClientState* client_state = 0;
              if (i != client_states.end())
                {
                  client_state = i->second;
                }
              else
                {
                  std::cout << "# allocating new ClientState" << std::endl;
                  client_state = new ClientState(client_id);
                  client_states[client_id] = client_state;
                }
              
              if (tokens.size() == 3 && tokens[2] == "stroke_begin")
                {
                  client_state->stroke_begin();
                }
              else if (tokens.size() == 3 && tokens[2] == "stroke_end")
                {
                  client_state->stroke_end();
                }
              else if (tokens.size() == 4 && tokens[2] == "set_brush")
                {
                  client_state->set_brush(tokens[3]);
                }
              else if (tokens.size() == 9 && tokens[2] == "set_generic_brush")
                {
                  client_state->set_generic_brush((BrushShape)atoi(tokens[3].c_str()),  // shape FIXME: could use name instead
                                                  atof(tokens[4].c_str()),  // radius
                                                  atoi(tokens[5].c_str()),  // spike
                                                  atof(tokens[6].c_str()),  // hardness
                                                  atof(tokens[7].c_str()),  // aspectratio
                                                  atof(tokens[8].c_str())); // angle
                }
              else if (tokens.size() == 4 && tokens[2] == "set_opacity")
                {
                  client_state->set_opacity(atoi(tokens[3].c_str()));
                }
              else if (tokens.size() == 6 && tokens[2] == "set_color")
                {
                  client_state->set_color(Color(atoi(tokens[3].c_str()), 
                                                atoi(tokens[4].c_str()), 
                                                atoi(tokens[5].c_str())));
                }
              else if (tokens.size() == 6 && tokens[2] == "dab")
                {
                  client_state->dab(atoi(tokens[3].c_str()), 
                                    atoi(tokens[4].c_str()),
                                    atoi(tokens[5].c_str()));
                }
              else
                {
                  std::cout << "# invalid command: " << cmd << std::endl;
                }
            }
          else
            {
              std::cout << "# invalid command: " << cmd << std::endl;
            }
        }
      else if (tokens[0] == "version")
        {
          if (tokens.size() == 2)
            {
              if (atoi(tokens[1].c_str()) != 0)
                {
                  std::cout << "# version mismatch: " << cmd << std::endl;
                  std::cout << "# upgrade your netbrush client" << std::endl;
                  exit(1);
                }
            }
          else
            {
              std::cout << "# invalid command: " << cmd << std::endl;
            }
        }
      else if (tokens[0] == "your_id")
        {
          if (tokens.size() == 2)
            {
              std::cout << "# my Id: " << atoi(tokens[1].c_str()) << std::endl;
            }
          else
            {
              std::cout << "# invalid command: " << cmd << std::endl;
            }
        }
      else
        {
          std::cout << "# invalid command: " << cmd << std::endl;
        }
    }
}

void update_network()
{
  if (tcpsock)
    {
      int num = 0;
      if ((num = SDLNet_CheckSockets(socketset, 0)) == -1)
        {
          printf("SDLNet_CheckSockets: %s\n", SDLNet_GetError());
          //most of the time this is a system error, where perror might help you.
          perror("SDLNet_CheckSockets");
        }
  
      if (num > 0)
        {
          if (SDLNet_SocketReady(tcpsock))
            {
              const int MAXLEN = 1024;
              int result;
              char msg[MAXLEN];

              result = SDLNet_TCP_Recv(tcpsock, msg, MAXLEN);
              if(result <= 0) 
                {
                  // TCP Connection is broken. (because of error or closure)
                  SDLNet_TCP_Close(tcpsock);
                  exit(1);
                }
              else 
                {
                  for(int i = 0; i < result; ++i)
                    {
                      if (msg[i] == '\n')
                        {
                          process_command(server_buffer);
                          //std::cout << server_buffer << std::endl;
                          server_buffer.clear();
                        }
                      else
                        {
                          server_buffer += msg[i];
                        }
                    }
                }
            }
        }
    }
}


void
draw_stroke(SDL_Surface* surface, const Stroke& stroke, DrawingParameter* param)
{
  SDL_Surface* brush = param->get_brush_surface();
  
  Stroke::Dabs dabs = stroke.get_interpolated_dabs(param->spacing, param->spacing);
  for(Stroke::Dabs::iterator i = dabs.begin(); i != dabs.end(); ++i)
    {
      SDL_Rect rect;
      rect.x = int(i->pos.x)-(brush->w/2);
      rect.y = int(i->pos.y)-(brush->h/2);
      rect.w = brush->w;
      rect.h = brush->h;
                  
      SDL_BlitSurface(brush, 0, surface, &rect);
    }
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
              std::string line = "clear\n";
              SDLNet_TCP_Send(tcpsock, const_cast<char*>(line.c_str()), line.length());
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

class RadiusCallback : public Callback
{
public:
  void operator()(float v) 
  {
    float radius = v * 100.0f + 0.1f;
    client_draw_param->generic_brush.radius = radius;
    std::cout << "Radius: " << radius << std::endl;
    brush_widget->set_brush(client_draw_param->generic_brush);
  }
};

class SpikeCallback : public Callback
{
public:
  void operator()(float v) 
  {
    int spikes = int(v*18) + 2;
    std::cout << "Spike: " << spikes << std::endl;
    client_draw_param->generic_brush.spikes = spikes;    
    brush_widget->set_brush(client_draw_param->generic_brush);
  }
};

class HardnessCallback : public Callback
{
public:
  void operator()(float v) 
  {
    float hardness = v;
    client_draw_param->generic_brush.hardness = v;
    std::cout << "Hardness: " << hardness << std::endl;
    brush_widget->set_brush(client_draw_param->generic_brush);
  }
};

class AspectRatioCallback : public Callback
{
public:
  void operator()(float v) 
  {
    float aspect_ratio = v*19.0f + 1.0f;
    client_draw_param->generic_brush.aspect_ratio = aspect_ratio;
    std::cout << "Aspect_Ratio: " << aspect_ratio << std::endl;
    brush_widget->set_brush(client_draw_param->generic_brush);
  }
};

class AngleCallback : public Callback
{
public:
  void operator()(float v) 
  {
    float angle = v * 360.0f;
    client_draw_param->generic_brush.angle = angle;
    std::cout << "Angle: " << angle << std::endl;
    brush_widget->set_brush(client_draw_param->generic_brush);
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
  
  screen_buffer = new ScreenBuffer(Rect(68, 0, screen->w - 128, screen->h));
  draw_ctx      = new DrawingContext(1024, 1024);
  stroke_buffer = new StrokeBuffer(1024, 1024);

  std::cout << "# clear screen" << std::endl;

  // clear screen
  draw_ctx->clear();

  std::cout << "# clear screen done" << std::endl;

  client_draw_param = new DrawingParameter();
  stroke_buffer->set_param(client_draw_param);
  
  if (argc == 3)
    {
      std::cout << "# connecting to: " << argv[1] << ":" << atoi(argv[2]) << std::endl;
      connect(argv[1], atoi(argv[2]));
    }
  else
    {
      std::cout << "# use '" << argv[0] << " HOSTNAME PORT' to connect a networking session" << std::endl;
    }

  
  widget_manager = new WidgetManager();
  {
    for(int y = 0; y < 10; ++y)
      for(int x = 0; x < 2; ++x)
        {
          widget_manager->add(new Button(Rect(Point(x*34, y*34), Size(34, 34))));
        }
  }

  {
    SDL_Rect color_rect;
    color_rect.x = 768;
    color_rect.y = 100;

    color_rect.w = 128;
    color_rect.h = 128;

    //widget_manager->add(new ColorSelector(&color_rect));
  }

  widget_manager->add(screen_buffer);

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
      update_network();
      widget_manager->update();
      SDL_Delay(10);
    }
  
  return 0;
}

/* EOF */

