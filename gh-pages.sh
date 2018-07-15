#!/bin/bash
set -e

rm -rf gh-pages || exit 0;

# compile JS using Elm
for example in Boxes; do
  lower=$(echo "$example" | tr '[:upper:]' '[:lower:]')
  cd examples/
  mkdir -p ../gh-pages/examples/$lower
  elm make $example.elm --yes --output ../gh-pages/examples/$lower/index.html
  cd ..
done

# init branch and commit
cd gh-pages
git init
git add .
git commit -m "Deploying to GH Pages"
git push --force "git@github.com:w0rm/elm-physics.git" master:gh-pages
