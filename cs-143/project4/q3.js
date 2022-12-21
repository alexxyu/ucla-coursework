db.laureates.aggregate([
  { $group: { _id: "$familyName", count: { $sum: 1 } } },
  { $match: { $and: [{ count: { $gte: 5 } }, { _id: { $ne: null } }] } },
  { $project: { "familyName": "$_id.en", "_id": 0 } }
]);
