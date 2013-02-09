ERL=erl
APP_NAME=erlmail
NODE_NAME=erlmail
VSN=0.0.6

all: ebin

.PHONY: ebin clean

ebin:
	$(ERL) -noshell -make -s erlang halt


clean:
	rm -fv ebin/*.beam


