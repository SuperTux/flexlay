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

#include <vector>
#include <iostream>
#include <sstream>
#include <fstream>
#include "client_connection.hpp"

extern std::vector<ClientConnection*> clients;
extern std::vector<std::string> drawing_history;
extern std::ofstream* outfile;

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

ClientConnection::ClientConnection(int id_, TCPsocket socket)
  : id(id_), 
    tcpsock(socket),
    invalid(false)
{
  buffer_pos = 0;
  full_client = false;
}
  
bool
ClientConnection::is_invalid()
{
  return invalid;
}

void
ClientConnection::update()
{
  if (invalid) return;

  if (SDLNet_SocketReady(tcpsock))
    {
      const int MAXLEN = 1024;
      char msg[MAXLEN];

      int result = SDLNet_TCP_Recv(tcpsock, msg, MAXLEN);
      if(result <= 0) 
        {
          // TCP Connection is broken. (because of error or closure)
          std::cout << "# Connection break, abort" << std::endl;
          invalid = true;
          return;
        }
      else 
        {
          for(int i = 0; i < result; ++i)
            {
              if (msg[i] == '\n')
                {
                  process_line(buffer);
                  buffer.clear();
                  buffer_pos = 0;
                }
              else
                {
                  buffer += msg[i];
                }
            }
        }
    }
}

void
ClientConnection::send_string(const std::string& line)
{
  if (full_client)
    {
      if (invalid) return;
        
      //std::cout << "Sending: '" << line << "' ... " << std::flush;
      int result = SDLNet_TCP_Send(tcpsock, const_cast<char*>(line.c_str()), line.length());
      if (result < int(line.length()))
        {
          // It may be good to disconnect sock because it is likely invalid now.
          printf( "Error: SDLNet_TCP_Send: '%s'\n", SDLNet_GetError() );
          invalid = true;
        }
      //std::cout << "done" << std::endl;
    }
}

void
ClientConnection::process_line(const std::string& line)
{
  if (invalid) return;
    
  std::vector<std::string> tokens = tokenize(line, ' ');
  if (tokens.size() == 2 && tokens[0] == "load")
    {
      for(int i = 0; i < int(clients.size()); ++i)
        {
          if (clients[i])
            {
              clients[i]->send_string("clear\n");
            }
        }
      std::cout << "# load unimplemented" << std::endl;
    }
  else if (tokens.size() == 2 && tokens[0] == "import")
    {
      std::cout << "# import unimplemented" << std::endl;
    }
  else if (tokens.size() == 2 && tokens[0] == "save")
    {
      for(int i = 0; i < int(tokens[1].size()); ++i)
        {
          if (tokens[1][i] == '/' || tokens[1][i] == '.')
            {
              tokens[1][i] = '.';
            }
        }
        
      std::ostringstream filename;
      filename << "images/" << tokens[1] << ".nbr";        

      int j = 1;
      std::string fname = filename.str();
      while (access(fname.c_str(), F_OK) == 0)
        {
          filename.str("");
          filename << "images/" << tokens[1] << "-" << j << ".nbr";
          fname = filename.str();
          j += 1;
        }

      std::cout << "# writing log to " << filename.str() << std::endl;
      std::ofstream out(fname.c_str());
      for(int i = 0; i < int(drawing_history.size()); ++i)
        out << drawing_history[i];
      out.close();
    }
  else if (tokens.size() == 2 && tokens[0] == "client_version")
    {
      if (atoi(tokens[1].c_str()) == 1)
        {
          full_client = true;
          for(int i = 0; i < int(drawing_history.size()); ++i)
            {
              clients.back()->send_string(drawing_history[i]);
            }
        }
    }
  else if (tokens.size() == 1 && tokens[0] == "clear")
    {
      std::ostringstream filename;
      filename << "sessions/session-" << time(NULL) << ".nbr";
        
      std::cout << "# writing log to " << filename.str() << std::endl;
      outfile->close();
      delete outfile;
      outfile = new std::ofstream(filename.str().c_str());

      drawing_history.clear();

      for(int i = 0; i < int(clients.size()); ++i)
        {
          if (clients[i])
            {
              clients[i]->send_string("clear\n");
            }
        }
    }
  else if (tokens.size() >= 1 && 
           (tokens[0] == "dab" ||
            tokens[0] == "stroke_begin" ||
            tokens[0] == "stroke_end"   ||
            tokens[0] == "set_brush"    ||
            tokens[0] == "set_generic_brush" ||
            tokens[0] == "set_color"    ||
            tokens[0] == "set_tool"     ||
            tokens[0] == "set_opacity" 
            ))
    {
      std::ostringstream str;
      str << "client " << id << " " << line << std::endl;
      //std::cout << "SERVER: " << str.str();
      drawing_history.push_back(str.str());
      (*outfile) << str.str() << std::flush;

      for(int i = 0; i < int(clients.size()); ++i)
        {
          if (clients[i])
            {
              clients[i]->send_string(str.str());
              // FIXME: write to file
            }
        }
    }
  else
    {
      std::cout << "# invalid command: " << line << std::endl;
    }
}

/* EOF */
