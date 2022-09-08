run-website:
	elm-live src/Main.elm

website:
	elm make src/Main.elm --optimize

deploy-website:
	cp index.html docs/index.html

test-worker:
	elm make src/Worker.elm --optimize --output elm.min.js

build-and-run-test-worker:
	make test-worker
	make run-test-worker

run-test-watcher:
	elm-test --watch

run-test-worker:
	node TestGenerator.js

clean:
	rm -f *.min.js
	rm -rf elm-stuff/
