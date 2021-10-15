SELECT S.id id
FROM Student S
INNER JOIN Takes T
ON S.id = T.stud_id
INNER JOIN Class C
ON T.class_id = C.id
GROUP BY S.id
ORDER BY SUM(credits) DESC
LIMIT 4;