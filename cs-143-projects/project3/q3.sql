WITH Counts AS (
    SELECT family_name, COUNT(*) AS prizes
    FROM Person, Prize
    WHERE id = winner_id
    GROUP BY family_name
)
SELECT family_name
FROM Counts
WHERE prizes >= 5;
