WITH RECURSIVE PrereqList(req, prereq) AS (
    (SELECT * FROM Prereq)
    UNION
    (SELECT P1.class_id, P2.prereq
    FROM Prereq P1, PrereqList P2
    WHERE P1.prereq_id = P2.req)
)
SELECT prereq FROM PrereqList WHERE req = 'BIO-399';