#!/bin/bash
set -euxo pipefail

version=${1:-}

if [ -z "$version" ]; then
  echo "Please set the desired version"
  exit 1
fi

if [[ -n $(git status --porcelain) ]]; then
  echo "Please commit all your changes"
  exit 1
fi

echo "Y" | elm bump
elm_version=$(grep -m1 version elm.json | awk -F: '{ print $2 }' | sed 's/[", ]//g')

if [ $version != $elm_version ]; then
  echo "Versions $elm_version and $version do not match!"
  exit 1
fi

git add elm.json
git commit -m "Bump to $version"
git push
last_commit=$(git rev-parse HEAD)

git rm -rf --ignore-unmatch .github benchmarks examples scripts tests elm-physics.gif
sed -i.bak "s+https://unsoundscapes.com/elm-physics/+https://unsoundscapes.com/elm-physics/$version/+g" README.md
sed -i.bak "s+https://github.com/w0rm/elm-physics/tree/main/+https://github.com/w0rm/elm-physics/tree/$last_commit/+g" README.md
rm README.md.bak
git add README.md
git commit -m "Release $version"
git tag -a $version -m "Release $version"
git push origin $version
elm publish

# restore the main branch
git checkout $last_commit
