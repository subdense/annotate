DATACOMMIT=`git rev-parse --short HEAD`
rm -rf tmp
mkdir tmp
cd tmp
git clone git@github.com:umrlastig/annotator.git
rm annotator/src/main/resources/config.json
cp ../config.json annotator/src/main/resources
cd annotator
APPCOMMIT=`git rev-parse --short HEAD`
sbt fullLinkJS
npm install
npm run build
cd ../..
git checkout gh-pages
rm -rf assets
mv tmp/annotator/dist/* .
git add -A
git commit -m "Deploy from app:$APPCOMMIT and data:$DATACOMMIT"
git push origin gh-pages
git checkout main

