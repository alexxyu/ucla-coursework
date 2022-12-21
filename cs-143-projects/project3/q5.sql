WITH OrgWinners AS (
    SELECT DISTINCT year
    FROM Prize, Organization
    WHERE winner_id = id
)
SELECT COUNT(*) years
FROM OrgWinners;
