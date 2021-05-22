trap 'kill %1; kill %2; kill %3; kill %4' SIGINT
python server.py Riley & python server.py Bernard & python server.py Juzang & python server.py Jaquez & python server.py Campbell
trap - SIGINT