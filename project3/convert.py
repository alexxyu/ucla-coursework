import json

# Load data
data = json.load(open("/home/cs143/data/nobel-laureates.json", "r"))

# We use sets to ensure there are no duplicates
Person = set()
Organization = set()
Prize = set()
Affiliation = set()

# Iterate through all the laureates
for laureate in data["laureates"]:
    id = laureate["id"]

    if "givenName" in laureate:
        # This is a person who won
        given_name = laureate["givenName"]["en"]
        try:
            family_name = laureate["familyName"]["en"]
        except KeyError:
            family_name = "\\N"
        gender = laureate["gender"]

        # Parse birth information
        try:
            birth = laureate["birth"]
            date = birth["date"]
            place = birth["place"]

            try:
                city = place["city"]["en"]
            except KeyError:
                city = "\\N"

            try:
                country = place["country"]["en"]
            except KeyError:
                country = "\\N"
        except KeyError:
            date = "\\N"
            city = "\\N"
            country = "\\N"
        
        # Person(id, given_name, family_name, gender, birth_date, birth_city, birth_country);
        Person.add(id + "\t" + given_name + "\t" + family_name + "\t" + gender + "\t" + date + "\t" + city + "\t" + country)
    else:
        # This is a organization who won
        org_name = laureate["orgName"]["en"]

        # Parse founding details
        try:
            founded = laureate["founded"]
            date = founded["date"]
            place = founded["place"]
            try:
                city = place["city"]["en"]
            except KeyError:
                city = "\\N"

            try:
                country = place["country"]["en"]
            except KeyError:
                country = "\\N"
        except KeyError:
            date = "\\N"
            city = "\\N"
            country = "\\N"
        
        # Organization(id, name, founded_date, founded_city, founded_country);
        Organization.add(id + "\t" + name + "\t" + date + "\t" + city + "\t" + country)

    # Iterate through prizes that were won
    prizes = laureate["nobelPrizes"]
    for prize in prizes:
        award_year = prize["awardYear"]
        category = prize["category"]["en"]
        sort_order = prize["sortOrder"]

        # Prize(prize_id, winner_id, year, category, sort_order);
        prize_id = str(len(Prize))
        Prize.add(prize_id + "\t" + id + "\t" + award_year + "\t" + category + "\t" + sort_order)

        # Parse affiliations of the winner
        if "affiliations" in prize:
            affiliations = prize["affiliations"]
            for affiliation in affiliations:
                name = affiliation["name"]["en"]
                try:
                    city = affiliation["city"]["en"]
                except KeyError:
                    city = "\\N"

                try:
                    country = affiliation["country"]["en"]
                except KeyError:
                    country = "\\N"

                # Affiliation(prize_id, name, city, country);
                Affiliation.add(prize_id + "\t" + name + "\t" + city + "\t" + country)

# Write formatted data to files
with open('Person.del', 'w') as f:
    f.write("\n".join(Person))

with open('Organization.del', 'w') as f:
    f.write("\n".join(Organization))

with open('Prize.del', 'w') as f:
    f.write("\n".join(Prize))

with open('Affiliation.del', 'w') as f:
    f.write("\n".join(Affiliation))
