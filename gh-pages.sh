#!/bin/bash
set -e

rm -rf gh-pages || exit 0;

mkdir -p gh-pages/examples

# compile JS using Elm
for i in boxes; do
  cd examples/$i
  mkdir -p ../gh-pages/examples/$i
  elm make Main.elm --yes --output ../../gh-pages/examples/$i/index.html
  cd ../..
done

# init branch and commit
cd gh-pages
git init
git add .
git commit -m "Deploying to GH Pages"
git push --force "git@github.com:w0rm/elm-physics.git" master:gh-pages
