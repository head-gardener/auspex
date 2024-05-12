default:
    @just --list

docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

repl *ARGS:
    cabal repl {{ARGS}}

fmt:
    treefmt

do *ARGS:
    ghcid -c "cabal repl lib:auspex-lib" --warnings -T {{ARGS}}

run:
    ghcid -c "cabal repl exe:auspex" --warnings -T :main

example:
    ghcid -c "cabal repl exe:example-app" --warnings -T :main

client:
    ghcid -c "cabal repl exe:auspex-client" --warnings -T :main

test:
    ghcid -c "cabal repl test:auspex-test" --test "Main.main"
