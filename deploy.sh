COMMIT=`git rev-parse --short HEAD`
sbt fullLinkJS
npm run build
git checkout gh-pages
rm -rf assets
mv dist/* .
git commit -m "Update to build from main:$COMMIT"
git push origin gh-pages
git checkout main

