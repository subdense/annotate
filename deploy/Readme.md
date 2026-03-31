
# Deployment scripts

For direct deployment on github pages by pushing the js build to a gh-pages branch

`deploy-test.sh` for testing only: use default `config.json` in `src/main/resources` (data repo for test: https://github.com/umrlastig/annotation-example.git)

`deploy-on-data-repo.sh` :
  - copy this script and the config file (if not there already) to the root of a deployment repository (that can be also the data repository, such as annotation-example); in case your data can not be public and your organisation has not a pro github subscription, you need a separate private data repository and public deploy repository
  - edit the config as needed:
    * 'url' must link to the data repo
    * 'base' must be the deployment repo name
    * 'annotationSetup' for each dataset (identified by its path in datasets.json), double array of annotation tags for each annotation step (example: [[tag1-step1,tag2-step1,tag3-step1],[tag1-step2,tag2-step2]])
  - make sure a gh-pages branch exists
  - .gitignore with `tmp` dir ignored
  - run the script: this builds the app and pushes it to the gh-pages branch; the deployed app is then available at https://$ORGANISATION.github.io/$REPOSITORY

