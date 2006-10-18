/*  $Id$
**   __      __ __             ___        __   __ __   __
**  /  \    /  \__| ____    __| _/_______/  |_|__|  | |  |   ____
**  \   \/\/   /  |/    \  / __ |/  ___/\   __\  |  | |  | _/ __ \
**   \        /|  |   |  \/ /_/ |\___ \  |  | |  |  |_|  |_\  ___/
**    \__/\  / |__|___|  /\____ /____  > |__| |__|____/____/\___  >
**         \/          \/      \/    \/                         \/
**  Copyright (C) 2005 Ingo Ruhnke <grumbel@gmx.de>
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

#include "globals.hpp"

TCPsocket tcpsock = 0;
SDLNet_SocketSet socketset;

std::string server_buffer;
DrawingContext*   draw_ctx          = 0;
DrawingParameter* client_draw_param = 0;
ScreenBuffer*     screen_buffer     = 0;
StrokeBuffer*     stroke_buffer     = 0;
WidgetManager*    widget_manager    = 0;

std::map<int, ClientState*> client_states;

SaturationValuePicker* saturation_value_picker =0;
HuePicker*   hue_picker =0;
AlphaPicker* alpha_picker = 0;
BrushWidget* brush_widget = 0;
Stroke* current_stroke = 0;

/* EOF */
