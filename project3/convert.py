import json

# load data
data = json.load(open("/home/cs143/data/nobel-laureates.json", "r"))

# get the id, givenName, and familyName of the first laureate
laureate = data["laureates"][0]
id = laureate["id"]
givenName = laureate["givenName"]["en"]
familyName = laureate["familyName"]["en"]

# print the extracted information
print(id + "\t" + givenName + "\t" + familyName)
