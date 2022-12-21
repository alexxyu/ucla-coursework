db.laureates.aggregate([
  { $match: { $and: [{ "givenName.en": "Marie" }, { "familyName.en": "Curie" }] } },
  { $project: { id: 1, _id: 0 } }
]);
