#!/bin/bash
chokidar "**/*.elm" --silent --initial -c "tput reset; elm make src/Main.elm --output=elm.js"
