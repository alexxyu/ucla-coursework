WITH StudentCreds AS (
    SELECT stud_id, SUM(credits) class_creds
    FROM Takes T
    INNER JOIN Class C
    ON T.class_id = C.id
    GROUP BY stud_id
)
SELECT id, tot_cred -
CASE
    WHEN class_creds IS NULL THEN 0
    ELSE class_creds
END credit_discrepency
FROM Student
LEFT OUTER JOIN StudentCreds
ON id = stud_id;