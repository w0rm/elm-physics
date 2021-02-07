#!/bin/bash
set -euxo pipefail

version=${1:-}

if [ -z "$version" ]; then
  version_path=""
else
  version_path="/$version"
fi

cd gh-pages

if [ "$(git rev-parse --abbrev-ref HEAD)" != "gh-pages" ]; then
  echo "Please checkout gh-pages branch"
  exit 1;
fi

if [[ -n $(git status --porcelain) ]]; then
  echo "Please commit all your changes"
  exit 1
fi

cd ../examples/src
for example in *.elm; do
  # rename CamelCase to snake-case
  lower=$( echo "${example%.*}" \
         | sed 's/\(.\)\([A-Z]\)/\1-\2/g' \
         | tr '[:upper:]' '[:lower:]' \
         )
  mkdir -p ../../gh-pages$version_path/examples/$lower
  elm make $example --optimize --output ../../gh-pages$version_path/examples/$lower/index.html
done
cp Duckling.obj.txt Duckling.png ../../gh-pages$version_path/examples/pod
cp ../elm-physics.gif ../../gh-pages$version_path/examples

cd ../../gh-pages
git add .
git commit -m "Deploying $version_path to GH Pages"
git push
