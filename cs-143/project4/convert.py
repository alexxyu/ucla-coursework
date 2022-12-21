import json

# load data
data = json.load(open("/home/cs143/data/nobel-laureates.json", "r"))

laureates = [json.dumps(laureate, indent=2) for laureate in data['laureates']]

with open('laureates.import', 'w') as f:
    f.write("\n".join(laureates))        
