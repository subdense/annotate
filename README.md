# A simple Annotation tool for SUBDENSE

## dev

Shell #1
```shell
sbt ~fastLinkJS
```
Shell #2
```shell
npm install
npm run dev
```

## build

Shell #1
```shell
sbt fullOptJS
npm run build
```

## cleanup script

To clean up a messy repo with duplicate annotations, use the dedicated script.
It uses environment variables *REPO_URL* and *GITHUB_TOKEN*.
It has two modes: dry-run and standard. The *dry-run* process simulates the process but does not push the results.
```shell
export REPO_URL="https://github.com/umrlastig/annotation-example.git"
export GITHUB_TOKEN="ghp_..."
sbt "backend/run --dry-run"
```
To actually commit and push the clean repo, use:
```shell
sbt "backend/run"
```

You can also activate the removal of small features (<10 m2):
```shell
sbt "backend/run --filter-small"
```

You can also use the dirty mode to keep the tmp directory (for debug use):
```shell
sbt "backend/run --dry-run --filter-small --dirty"
```


## TODO
- sorting of objects for annotation?

### Layers in Personal dashboard
- annotated vs not annotated
- annotated link types
- annotated change types

### Layers in Global dashboard
- annotated vs not annotated
- annotated types (majority?)
- annotation conflict?

## Acknowledgments
Loading_2.gif from KopiteCowboy, CC BY-SA 4.0 <https://creativecommons.org/licenses/by-sa/4.0>, via Wikimedia Commons