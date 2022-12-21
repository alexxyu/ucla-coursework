db.laureates.aggregate([
  { $unwind: "$nobelPrizes" },
  { $match: { "orgName": { $ne: null } } },
  { $group: { _id: "$nobelPrizes.awardYear" } },
  { $count: "years" }
]);
