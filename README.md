# inzight state management package

The idea is for this package to provide a back-end that can easily be run on any server (locally or remote) and connect to a web-based app using e.g., ReactJS or similar.

## Basic example

Start running a 'plumber' instance:

```r
plumber::pr('inst/server/app.R') |> plumber::pr_run(port = 4567)
```

Next call the API for state management:

```bash
# get an initial state
curl localhost:4567/new | jq . > state0.json

cat state0.json
# {
#   "docs": [
#     {}
#   ],
#   "controls": {
#     "v1": {},
#     "v2": {},
#     "g1": {},
#     "g2": {}
#   }
# }

# load some data
cat << EOF > action.json
{
  "action": "LOAD_DATA",
  "payload": {
    "file": "https://www.stat.auckland.ac.nz/~wild/data/data_from_iNZight/Census%20at%20School-500.csv"
  }
}
EOF

curl -X POST localhost:4567/dispatch \
    -F "state=$(cat state0.json)" \
    -F "action=$(cat action.json)" | jq .
# {
#   "docs": [
#     {
#       "path": [
#         "/tmp/RtmpH7dawk/NCMSCTROAGCKOEOQAHTF"
#       ],
#       "name": [
#         "Census%20at%20School-500"
#       ],
#       "colnames": [
#         "cellsource",
#         "rightfoot",
#         "travel",
#         "getlunch",
#         "height",
#         "gender",
#         "age",
#         "year",
#         "armspan",
#         "cellcost"
#       ]
#     }
#   ],
#   "controls": {
#     "v1": {},
#     "v2": {},
#     "g1": {},
#     "g2": {}
#   }
# }
```

The data is stored on the server and only necessary bits will be passed to the UI (e.g., to view a spreadsheet). The session will (eventually) generate a user key (which may be associated with, for example, a log-in) allowing them---and only them---to view their data.
