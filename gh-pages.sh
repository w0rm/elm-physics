#!/bin/bash
set -e

rm -rf gh-pages || exit 0;

# Compile the examples
cd examples/
for example in *.elm; do
  # rename CamelCase to snake-case
  lower=$( echo "${example%.*}" \
         | sed 's/\(.\)\([A-Z]\)/\1-\2/g' \
         | tr '[:upper:]' '[:lower:]' \
         )
  mkdir -p ../gh-pages/examples/$lower
  elm make $example --optimize --output ../gh-pages/examples/$lower/index.html
done

# Deploy to GH Pages
cd ../gh-pages
git init
git add .
git commit -m "Deploying to GH Pages"
git push --force "git@github.com:w0rm/elm-physics.git" master:gh-pages
