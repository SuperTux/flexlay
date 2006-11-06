/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software; you can redistribute it and/or
**  modify it under the terms of the GNU General Public License
**  as published by the Free Software Foundation; either version 2
**  of the License, or (at your option) any later version.
**
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
** 
**  You should have received a copy of the GNU General Public License
**  along with this program; if not, write to the Free Software
**  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
**  02111-1307, USA.
*/

#ifndef HEADER_GLOBALS_HPP
#define HEADER_GLOBALS_HPP

#include "SDL_net.h"

#include <map>
#include <vector>
#include <string>

class Controller;
class StrokeBuffer;
class ScreenBuffer;
class Stroke;
class ClientState;
class DrawingContext;
class DrawingParameter;
class WidgetManager;
class ServerConnection;
class Scrollbar;
class Navigation;
class InputDevice_XInput;

extern SDLNet_SocketSet socketset;

extern Scrollbar* horizontal_scrollbar;
extern Scrollbar* vertical_scrollbar;
extern DrawingContext*   draw_ctx;
extern DrawingParameter* client_draw_param;
extern std::map<int, ClientState*> client_states;
extern ScreenBuffer*     screen_buffer;
extern StrokeBuffer*     stroke_buffer;
extern WidgetManager* widget_manager;

extern ServerConnection* server;

extern Navigation* navigation;
extern Controller* controller;
extern InputDevice_XInput* xinput;

#endif

/* EOF */
