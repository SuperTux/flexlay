#include <iostream>
#include <stdexcept>
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
#include "brush_widget.hpp"
#include "navigation.hpp"
#include "server_connection.hpp"
#include "command_line.hpp"
#include "text_view.hpp"
#include "widget/slider_widget.hpp"
#include "controller.hpp"

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
          puts("Quitting");
          exit(0);
          break;

        case SDL_KEYDOWN:
          if (event.key.keysym.sym == SDLK_k)
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
          else if (event.key.keysym.sym == SDLK_F11)
            {
              if (SDL_WM_ToggleFullScreen(screen) == 0) {
                std::cout << "Failed to toggle fullscreen mode: " << SDL_GetError() << std::endl;
                //quit(1);
              }
            }
          else if (event.key.keysym.sym == SDLK_u)
            {
              navigation->update();
            }
          else if (event.key.keysym.sym == SDLK_ESCAPE)
            {
              exit(EXIT_SUCCESS);
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

int main(int argc, char** argv)
{
  try {
    bool fullscreen = false;
    int screen_width  = 800;
    int screen_height = 600;
    int canvas_width  = 2048;
    int canvas_height = 2048;
    std::string hostname;
    std::string port     = "4711";
    int rest_arg_count = 0;

    CommandLine argp;

    argp.add_usage("[OPTIONS] HOSTNAME PORT");
    argp.add_group("Display:");
    argp.add_option('g', "geometry",  "WIDTHxHEIGHT", "Set the windows size to WIDTHxHEIGHT");
    argp.add_option('c', "canvas",    "WIDTHxHEIGHT", "Set the size of the paintable canvas to WIDTHxHEIGHT");
    argp.add_option('f', "fullscreen", "",            "Start the application in fullscreen mode");
    argp.add_option('w', "window",     "",            "Start the application in window mode");
    argp.add_option('v', "version",    "",            "Display the netBrush version");
    argp.add_option('h', "help",       "",            "Show this help text");

    argp.parse_args(argc, argv);

    while(argp.next())
      {
        switch(argp.get_key())
          {
          case 'g':
            {
              if (sscanf(argp.get_argument().c_str(), "%dx%d",
                         &screen_width, &screen_height) == 2)
                {
                  std::cout << "Geometry: " << screen_width << "x" << screen_height << std::endl;
                }
              else
                {
                  throw std::runtime_error("Geometry option '-g' requires argument of type {WIDTH}x{HEIGHT}");
                }
            }
            break;

          case 'c':
            {
              if (sscanf(argp.get_argument().c_str(), "%dx%d",
                         &canvas_width, &canvas_height) == 2)
                {
                  std::cout << "Geometry: " << canvas_width << "x" << canvas_height << std::endl;
                }
              else
                {
                  throw std::runtime_error("Canvas option '-c' requires argument of type {WIDTH}x{HEIGHT}");
                }
            }
            break;

          case 'f':
            fullscreen = true;
            break;

          case 'w':
            fullscreen = false;
            break;

          case 'h':
            argp.print_help();
            return 0;
            break;

          case 'v':
            std::cout << "netBrush 0.1.0" << std::endl;
            break;

          case CommandLine::REST_ARG:
            //std::cout << "Rest: " << argp.get_argument() << std::endl;
            if (rest_arg_count == 0)
              {
                hostname = argp.get_argument();
                rest_arg_count += 1;
              }
            else if (rest_arg_count == 1)
              {
                port = argp.get_argument();
                rest_arg_count += 1;
              }
            else
              {
                std::cout << "Invalide argument " << argp.get_argument() << std::endl;
                exit(EXIT_FAILURE);
              }
            break;
          }
      }

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

    Uint32 flags = SDL_HWSURFACE;
    if (fullscreen)
      flags |= SDL_FULLSCREEN;
    screen = SDL_SetVideoMode(screen_width, screen_height, 32, flags); 
    if (screen == 0)
      printf("SDL_SetVideoMode: %s\n", SDL_GetError());
    SDL_WM_SetCaption("netBrush", "netBrush");

    // 18 is scrollbar
    screen_buffer = new ScreenBuffer(Rect(38, 2, screen->w - 128 - 18 - 2 - 2, screen->h - 16 - 4 - 38)); 
    draw_ctx      = new DrawingContext(canvas_width, canvas_height);
    stroke_buffer = new StrokeBuffer(canvas_width, canvas_height);


    //std::cout << "# clear screen" << std::endl;

    // clear screen
    draw_ctx->clear();

    //std::cout << "# clear screen done" << std::endl;

    client_draw_param = new DrawingParameter();
    stroke_buffer->set_param(client_draw_param);
  
    server = new ServerConnection();
    if (!hostname.empty() && !port.empty())
      {
        std::cout << "Connecting to: " << hostname << ":" << atoi(port.c_str()) << std::endl;
        server->connect(hostname.c_str(), atoi(port.c_str()));
        std::ostringstream title_line;
        title_line << "netBrush - online: " << hostname << ":" << atoi(port.c_str());
        SDL_WM_SetCaption(title_line.str().c_str(), "netBrush");
      }
    else
      {
        std::cout << "# use '" << argv[0] << " HOSTNAME PORT' to connect a networking session" << std::endl;
        SDL_WM_SetCaption("netBrush - offline mode", "netBrush");
      }
  
    widget_manager = new WidgetManager();
    controller     = new Controller();

    widget_manager->add(new TextView(Rect(38, screen->h - 38,
                                          screen->w - 128 - 18 - 2 - 2, screen->h)));

    widget_manager->add(navigation = new Navigation(Rect(Point(screen->w - 128 - 2, screen->h - 128 - 2),
                                                         Size(128, 128))));
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
                        new Scrollbar(0, canvas_height, screen_buffer->get_rect().get_height(), Scrollbar::VERTICAL,
                                      Rect(screen->w - 128 - 16 - 2 - 2, 2,
                                           screen->w - 128 - 2 - 2, screen->h - 16 - 4 - 38)));

    widget_manager->add(horizontal_scrollbar = 
                        new Scrollbar(0, canvas_width, screen_buffer->get_rect().get_width(), Scrollbar::HORIZONTAL,
                                      Rect(38, screen->h - 16 - 2 - 38,
                                           screen->w - 128 - 18 - 2 - 2, screen->h - 2 - 38)));
    
    brush_widget = new BrushWidget(Rect(Point(screen->w-128, 128+24+24), Size(128, 128)));
    
    widget_manager->add(brush_widget);

    // Main Loop
    while(true)
      {
        process_events();
        server->update();
        widget_manager->update();
        SDL_Delay(10);
      }
  } catch(std::exception& err) {
    std::cout << "Exception: " << err.what() << std::endl;
  }
  
  return 0;
}

/* EOF */

