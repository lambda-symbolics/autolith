/* Native TCP policy probe. Report through the inherited parent connection. */
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <unistd.h>

int main(int argc, char **argv) {
  if (argc != 2) return 64;
  struct sockaddr_in address = {0};
  address.sin_family = AF_INET;
  address.sin_port = htons((unsigned short)strtoul(argv[1], NULL, 10));
  address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  int connection = socket(AF_INET, SOCK_STREAM, 0);
  int connected = connection >= 0 &&
    connect(connection, (struct sockaddr *)&address, sizeof(address)) == 0;
  if (connection >= 0) close(connection);
  dprintf(0, "%d %d\n", connected, (int)getpid());
  for (;;) pause();
}
