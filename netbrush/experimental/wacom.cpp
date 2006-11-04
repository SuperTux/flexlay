#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/XInput.h>

#include <iostream>
#include "SDL.h"
#include "SDL_syswm.h"
#include "input_device_xinput.hpp"

SDL_SysWMinfo syswm;

int main()
{
  bool fullscreen = false;
  int screen_width = 640;
  int screen_height = 480;
  SDL_Surface* sdl_screen = NULL;

  if(SDL_Init(SDL_INIT_VIDEO)== -1) {
    printf("SDL_Init: %s\n", SDL_GetError());
    exit(1);
  }
  atexit(SDL_Quit);

  Uint32 flags = SDL_HWSURFACE;
  if (fullscreen)
    flags |= SDL_FULLSCREEN;
  sdl_screen = SDL_SetVideoMode(screen_width, screen_height, 32, flags); 
  if (sdl_screen == 0)
    printf("SDL_SetVideoMode: %s\n", SDL_GetError());
  SDL_WM_SetCaption("netBrush", "netBrush");

  SDL_VERSION(&syswm.version); // this is important!
  if (SDL_GetWMInfo(&syswm) == -1)
    {
      std::cout << "Couldn't get WM info " << std::endl;
    }

  syswm.info.x11.lock_func();
  InputDevice_XInput xinput(syswm.info.x11.display, "gstylus");
  syswm.info.x11.unlock_func();

  SDL_EventState(SDL_SYSWMEVENT, SDL_ENABLE);

  while(true)
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


            case SDL_MOUSEBUTTONDOWN:
              std::cout << "Mouse down: " << int(event.button.button) << std::endl;
              break;

            case SDL_MOUSEBUTTONUP:
              std::cout << "Mouse up: " << int(event.button.button) << std::endl;
              break;

            case SDL_MOUSEMOTION:
              //std::cout << "Mouse motion: " << event.motion.x << " " << event.motion.y << std::endl;
              break;

            case SDL_SYSWMEVENT:
              //std::cout << "Sysevent" << std::endl;
              xinput.on_xevent(event.syswm.msg->event.xevent);
              break;
            }
        }
    }

  return 0;
}
