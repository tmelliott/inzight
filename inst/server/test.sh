# get an initial state
curl localhost:4567/new | jq . > 'state0.json'

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
