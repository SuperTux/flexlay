#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include "SDL.h"
#include "SDL_net.h"

#ifdef WIN32
#include  <io.h>
#define access _access
#define F_OK   0
#endif

class ClientConnection;

std::vector<ClientConnection*> clients;
SDLNet_SocketSet socketset;

TCPsocket serversock;

std::vector<std::string> drawing_history;

std::ofstream* outfile = 0;

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

class ClientConnection
{
public:
  int id;
  TCPsocket   tcpsock;
  int         buffer_pos;
  std::string buffer;
  bool        invalid;
  bool        full_client;
public:
  ClientConnection(int id_, TCPsocket socket)
    : id(id_), 
      tcpsock(socket),
      invalid(false)
  {
    buffer_pos = 0;
    full_client = false;
  }
  
  bool is_invalid()
  {
    return invalid;
  }

  void update()
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

  void send_string(const std::string& line)
  {
    if (full_client)
      {
        if (invalid) return;

        int result = SDLNet_TCP_Send(tcpsock, const_cast<char*>(line.c_str()), line.length());
        if (result < int(line.length()))
          {
            // It may be good to disconnect sock because it is likely invalid now.
            printf( "Error: SDLNet_TCP_Send: '%s'\n", SDLNet_GetError() );
            invalid = true;
          }
      }
  }

  void process_line(const std::string& line)
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
              tokens[0] == "set_generic_brush"    ||
              tokens[0] == "set_color"    ||
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
};

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
      std::cout << "# listening on: " << argv[1] << std::endl;
      connect(atoi(argv[1]));
    }
  else
    {
      std::cout << "Usage: " << argv[0] << " PORT" << std::endl;
    }

  outfile->close();
  delete outfile;

  return 0;
}
