db.laureates.aggregate([
  { $unwind: "$nobelPrizes" },
  { $unwind: "$nobelPrizes.affiliations" },
  { $match: { "nobelPrizes.affiliations.name.en": "CERN" } },
  { $group: { "_id": "$nobelPrizes.affiliations.country.en" } }
]);
