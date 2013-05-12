HSCC=ghc

OPTS=-Wall
DAEMON_NAME=nlcd
SERVER_NAME=nlsd
CLIENT_NAME=nlc

all: daemon server client

daemon: Daemon.hs
	$(HSCC) $(OPTS) --make Daemon.hs -o $(DAEMON_NAME)

server: Main.hs
	$(HSCC) $(OPTS) --make Main.hs -o $(SERVER_NAME)

client: Client.hs
	$(HSCC) $(OPTS) --make Client.hs -o $(CLIENT_NAME)


clean:
	./clean.sh
