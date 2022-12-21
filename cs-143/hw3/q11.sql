WITH Count2009 AS (
    SELECT stud_id id, COUNT(*) class_count
    FROM Takes
    WHERE year = 2009
    GROUP BY stud_id
), Count2010 AS (
    SELECT stud_id id, COUNT(*) class_count
    FROM Takes
    WHERE year = 2010
    GROUP BY stud_id
)
SELECT Count2009.id id
FROM Count2009
LEFT OUTER JOIN Count2010
ON Count2009.id = Count2010.id
WHERE Count2009.class_count > Count2010.class_count 
   OR Count2010.class_count IS NULL;