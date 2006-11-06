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

#ifndef HEADER_SERVER_CONNECTION_HPP
#define HEADER_SERVER_CONNECTION_HPP

#include <string>
#include "SDL_net.h"

class Stroke;
class DrawingParameter;

/** */
class ServerConnection
{
private:
  TCPsocket tcpsock;
  SDLNet_SocketSet socketset;
  std::string buffer;

public:
  ServerConnection();
  ~ServerConnection();

  void connect(const char* hostname, Uint16 port);

  /** Send a string to the server, you must add the trailing newline
      yourself to \a str */
  void send(const std::string& str);
  void send_stroke(const Stroke& stroke, DrawingParameter* param);
  void update();
  void process_command(const std::string& cmd);
private:
  ServerConnection (const ServerConnection&);
  ServerConnection& operator= (const ServerConnection&);
};

#endif

/* EOF */
