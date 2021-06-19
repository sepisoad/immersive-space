# =======================
# my elm app makefile
# @sepisoad
# =======================

.PHONY: build rebuild

clean:
	rm -rf build

copy:
	mkdir build
	cp -r static/* build/

build:
	elm make src/Main.elm --output=build/elm.js

rebuild:
	$(MAKE) clean
	$(MAKE) copy
	elm make src/Main.elm --output=build/elm.js

live-mac:
	$(MAKE) clean
	$(MAKE) copy
	elm-live src/Main.elm --pushstate --host=192.168.5.129 --port=1987 --start-page=index.html --dir=build -- --output=build/elm.js

live-linux:
	$(MAKE) clean
	$(MAKE) copy
	elm-live src/Main.elm --pushstate --host=192.168.5.125 --port=1987 --start-page=index.html --dir=build -- --output=build/elm.js

live:
	$(MAKE) clean
	$(MAKE) copy
	elm-live src/Main.elm --pushstate --port=1987 --start-page=index.html --dir=build -- --output=build/elm.js

install-elm-lang-server:
	npm install -g @elm-tooling/elm-language-server