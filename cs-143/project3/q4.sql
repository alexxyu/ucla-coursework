WITH UniqueLocations AS (
    SELECT DISTINCT city, country
    FROM Affiliation
    WHERE affiliation_name = 'University of California'
)
SELECT COUNT(*) locations
FROM UniqueLocations;
