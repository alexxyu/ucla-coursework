WITH Count19 AS (
    SELECT stud_id id, COUNT(*) class_count
    FROM Takes
    WHERE year = 2009
    GROUP BY stud_id
), Count20 AS (
    SELECT stud_id id, COUNT(*) class_count
    FROM Takes
    WHERE year = 2010
    GROUP BY stud_id
)
SELECT Count19.id id
FROM Count19
LEFT OUTER JOIN Count20
ON Count19.id = Count20.id
WHERE Count19.class_count >= Count20.class_count 
   OR Count20.class_count IS NULL;