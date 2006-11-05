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

#include <iostream>
#include <sstream>
#include <stdlib.h>
#include "globals.hpp"
#include "client_state.hpp"
#include "color.hpp"
#include "controller.hpp"
#include "drawing_context.hpp"
#include "drawing_parameter.hpp"
#include "server_connection.hpp"

static std::vector<std::string>
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

ServerConnection::ServerConnection()
  : tcpsock(0),
    socketset(0)
{
}

ServerConnection::~ServerConnection()
{
}

void
ServerConnection::send(const std::string& str)
{
  if (tcpsock)
    {
      int result = SDLNet_TCP_Send(tcpsock, const_cast<char*>(str.c_str()), str.length());
      if(result < int(str.length()))
        {
          printf( "SDLNet_TCP_Send: %s\n", SDLNet_GetError() );
          // It may be good to disconnect sock because it is likely invalid now.
        }     
    }
  else
    { // Not connected, so directly procses the command without a
      // round trip through the server
      std::string tmp;
      for(std::string::size_type i = 0; i < str.length(); ++i)
        {
          if (str[i] == '\n')
            {
              process_command("client 0 " + tmp);
              tmp.clear();
            }
          else
            tmp += str[i];
        }
    }  
}

void
ServerConnection::connect(const char* hostname, Uint16 port)
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

void
ServerConnection::update()
{
  if (!tcpsock) return;

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
                      process_command(buffer);
                      //std::cout << server_buffer << std::endl;
                      buffer.clear();
                    }
                  else
                    {
                      buffer += msg[i];
                    }
                }
            }
        }
    }
}

void
ServerConnection::process_command(const std::string& cmd)
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
                  //std::cout << "# allocating new ClientState" << std::endl;
                  client_state = new ClientState(client_id);
                  client_states[client_id] = client_state;
                }
              
              if (tokens.size() == 3 && tokens[2] == "stroke_begin")
                {
                  client_state->stroke_begin();
                }
              else if (tokens.size() == 9 && tokens[2] == "copy_region")
                { // copy_region X1 Y2 X2 Y2 TARGET_X TARGET_Y
                  client_state->copy_region(Rect(atoi(tokens[3].c_str()),
                                                 atoi(tokens[4].c_str()),
                                                 atoi(tokens[5].c_str()),
                                                 atoi(tokens[6].c_str())),
                                            Point(atoi(tokens[7].c_str()),
                                                  atoi(tokens[8].c_str())));
                }
              else if (tokens.size() == 7 && tokens[2] == "fill_rect")
                { // fill_rect X1 Y1 X2 Y2
                  client_state->fill_rect(Rect(atoi(tokens[3].c_str()),
                                               atoi(tokens[4].c_str()),
                                               atoi(tokens[5].c_str()),
                                               atoi(tokens[6].c_str())));
                }
              else if (tokens.size() == 7 && tokens[2] == "draw_line")
                { // draw_line X1 Y1 X2 Y2
                  client_state->draw_line(Point(atoi(tokens[3].c_str()),
                                                atoi(tokens[4].c_str())),
                                          Point(atoi(tokens[5].c_str()),
                                                atoi(tokens[6].c_str())));
                }
              else if (tokens.size() == 6 && tokens[2] == "fill_circle")
                { // fill_circle X Y RADIUS
                  client_state->fill_circle(Point(atoi(tokens[3].c_str()),
                                                  atoi(tokens[4].c_str())),
                                            atoi(tokens[5].c_str()));
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
              else if (tokens.size() == 4 && tokens[2] == "set_tool")
                {
                  client_state->set_tool(static_cast<DrawingParameter::Tool>(atoi(tokens[3].c_str())));
                }
              else if (tokens.size() == 6 && tokens[2] == "set_color")
                {
                  client_state->set_color(Color(atoi(tokens[3].c_str()), 
                                                atoi(tokens[4].c_str()), 
                                                atoi(tokens[5].c_str())));
                }
              else if (tokens.size() >= 3 && tokens[2] == "message")
                {
                  std::string str;
                  for(int i = 3; i < int(tokens.size()); ++i)
                    str += tokens[i] + " ";
                  controller->puts(str);
                }
              else if (tokens.size() == 6 && tokens[2] == "dab")
                {
                  client_state->dab(atoi(tokens[3].c_str()), 
                                    atoi(tokens[4].c_str()),
                                    atoi(tokens[5].c_str()),
                                    1.0f);
                }
              else if (tokens.size() == 7 && tokens[2] == "dab")
                {
                  client_state->dab(atoi(tokens[3].c_str()), 
                                    atoi(tokens[4].c_str()),
                                    atoi(tokens[5].c_str()),
                                    atof(tokens[6].c_str()));
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
              //std::cout << "# my Id: " << atoi(tokens[1].c_str()) << std::endl;
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

void
ServerConnection::send_stroke(const Stroke& stroke, DrawingParameter* param)
{
  const Stroke::Dabs& dabs = stroke.get_dabs();

  std::stringstream str;
  if (param->get_brush().empty())
    str << "set_generic_brush " 
        << param->generic_brush.shape << " "
        << param->generic_brush.radius << " "
        << param->generic_brush.spikes << " "
        << param->generic_brush.hardness << " "
        << param->generic_brush.aspect_ratio << " "
        << param->generic_brush.angle << " "
        << std::endl;
  else
    str << "set_brush " << param->get_brush() << std::endl;
  str << "set_tool " << param->tool << std::endl; 
  str << "set_opacity " << int(param->opacity) << std::endl;
  str << "set_color "
      << int(param->color.r) << " " 
      << int(param->color.g) << " " 
      << int(param->color.b) << std::endl;
  str << "stroke_begin" << std::endl;
  for(Stroke::Dabs::const_iterator i = dabs.begin(); i != dabs.end(); ++i)
    {
      str << "dab " << i->time << " " << i->pos.x << " " << i->pos.y << " " << i->pressure << std::endl;
    }
  str << "stroke_end" << std::endl;
      
  send(str.str());
}

/* EOF */
