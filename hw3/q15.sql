WITH StudentCreds AS (
    SELECT stud_id, SUM(credits) class_creds
    FROM Takes T
    INNER JOIN Class C
    ON T.class_id = C.id
    GROUP BY stud_id
)
SELECT stud_id, tot_cred -
CASE
    WHEN class_creds IS NULL THEN 0
    ELSE class_creds
END credit_discrepency
FROM Student S
INNER JOIN StudentCreds SC
ON S.id = SC.stud_id;