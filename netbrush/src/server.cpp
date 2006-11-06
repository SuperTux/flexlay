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

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include "SDL.h"
#include "SDL_net.h"
#include "client_connection.hpp"
#include "command_line.hpp"

#ifdef WIN32
#include  <io.h>
#define access _access
#define F_OK   0
#endif

std::vector<ClientConnection*> clients;
SDLNet_SocketSet socketset;

TCPsocket serversock;

std::vector<std::string> drawing_history;

std::ofstream* outfile = 0;

void accept_connections()
{
  if (SDLNet_SocketReady(serversock))
    {
      TCPsocket client = SDLNet_TCP_Accept(serversock);
      if (client)
        {
          clients.push_back(new ClientConnection(clients.size()+1, client));

          std::stringstream out;
          out << "version 0" << std::endl;
          out << "your_id " << clients.size() << std::endl;
          std::string str = out.str();
          SDLNet_TCP_Send(client, const_cast<char*>(str.c_str()), str.length()+1);

          IPaddress* ip = SDLNet_TCP_GetPeerAddress(client);

          const char* host;
          if((host = SDLNet_ResolveIP(ip)) != 0)
            std::cout << "# Got client connection from " << host << " " << ip->port << std::endl;
          else
            std::cout << "# Got client connection from " << ip->host << " " << ip->port << std::endl;
          
          int numused = SDLNet_TCP_AddSocket(socketset, client);
          if (numused == -1) {
            printf("SDLNet_AddSocket: %s\n", SDLNet_GetError());
            // perhaps you need to restart the set and make it bigger...
          } 
          else
            {
              std::cout << "# Sockets used: " << numused << std::endl;
            }
        }
    }
}

void connect(Uint16 port)
{
  IPaddress ip;
  
  if(SDLNet_ResolveHost(&ip,NULL,port)==-1) {
    printf("SDLNet_ResolveHost: %s\n", SDLNet_GetError());
    exit(1);
  }
  serversock = SDLNet_TCP_Open(&ip);
  if(!serversock) {
    printf("SDLNet_TCP_Open: %s\n", SDLNet_GetError());
    exit(2);
  }

  socketset = SDLNet_AllocSocketSet(32);
  if (!socketset) {
    printf("SDLNet_AllocSocketSet: %s\n", SDLNet_GetError());
    exit(1); //most of the time this is a major error, but do what you want.
  }

  SDLNet_TCP_AddSocket(socketset, serversock);

  while(true)
    {
      int num = 0;
      if ((num = SDLNet_CheckSockets(socketset, 10000)) == -1)
        {
          printf("SDLNet_CheckSockets: %s\n", SDLNet_GetError());
          //most of the time this is a system error, where perror might help you.
          perror("SDLNet_CheckSockets");
        }
      else
        { 
          accept_connections();
       
          for(int i = 0; i < int(clients.size()); ++i)
            {
              if (clients[i])
                clients[i]->update();
            }

          for(int i = 0; i < int(clients.size()); ++i)
            {
              if (clients[i] && clients[i]->is_invalid())
                {
                  std::cout << "# client " << clients[i]->id << " got disconnected" << std::endl;
                  SDLNet_TCP_DelSocket(socketset, clients[i]->tcpsock); 
                  SDLNet_TCP_Close(clients[i]->tcpsock);
                  delete clients[i];
                  clients[i] = 0;
                }
            }
        }
    }
}

int main(int argc, char** argv)
{
  try {
    std::string port;
    CommandLine argp;

    argp.add_usage("[OPTIONS] PORT");
    argp.add_group("General:");
    argp.add_option('v', "version", "", "Print the netBrush server version");
    argp.add_option('h', "help", "", "Print this help");

    argp.parse_args(argc, argv);
    while(argp.next())
      {
        switch(argp.get_key())
          {
          case 'h':
            argp.print_help();
            return 0;
            break;
            
          case 'v':
            std::cout << "netBrush Server 0.1.0" << std::endl;
            return 0;
            break;
            
          case CommandLine::REST_ARG:
            if (!port.empty())
              {
                std::cout << "Invalid argument: " << argp.get_argument() << std::endl;
                return 1;
              }
            else
              {
                port = argp.get_argument();
              }
            break;
          }
      }

    if (port.empty())
      {
        argp.print_help();
        return 1;
      }

    if(SDL_Init(0)==-1) {
      printf("SDL_Init: %s\n", SDL_GetError());
      exit(1);
    }

    if(SDLNet_Init()==-1) {
      printf("SDLNet_Init: %s\n", SDLNet_GetError());
      exit(2);
    }

    atexit(SDL_Quit);
    atexit(SDLNet_Quit);

    std::ostringstream filename;
    filename << "sessions/session-" << time(NULL) << ".nbr";

    std::cout << "# writing log to " << filename.str() << std::endl;
    outfile = new std::ofstream(filename.str().c_str());

    if (argc == 2)
      {
        std::cout << "# listening on: " << port << std::endl;
        connect(atoi(port.c_str()));
      }
    else
      {
        std::cout << "Usage: " << argv[0] << " PORT" << std::endl;
      }

    outfile->close();
    delete outfile;
  } catch (std::exception& err) {
    std::cout << "Exception: " << err.what() << std::endl;
  }
  return 0;
}
