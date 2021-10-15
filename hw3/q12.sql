WITH StudentCredits AS (
    SELECT S.id stud_id, year, SUM(credits) tot_cred
    FROM Student S
    INNER JOIN Takes T
    ON S.id = T.stud_id
    INNER JOIN Class C
    ON T.class_id = C.id
    GROUP BY S.id
)
SELECT stud_id, year max_credit_year
FROM StudentCredits S1
WHERE tot_cred = (
    SELECT MAX(tot_cred) 
    FROM StudentCredits S2
    WHERE S1.stud_id = S2.stud_id
    GROUP BY S1.stud_id)