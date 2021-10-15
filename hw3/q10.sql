WITH SchoolCount AS (
    SELECT D.dept, num_studs, num_insts,
    CASE
        WHEN D.dept = 'Comp. Sci.' or D.dept = 'Elec. Eng.' THEN 'Engineering'
        ELSE 'L&S'
    END school
    FROM Department D,
        (SELECT dept, COUNT(*) num_studs FROM Student GROUP BY dept) S,
        (SELECT dept, COUNT(*) num_insts FROM Instructor GROUP BY dept) I
    WHERE S.dept = D.dept AND I.dept = D.dept
)
SELECT school, SUM(num_studs) num_studs, SUM(num_insts) num_insts
FROM SchoolCount 
GROUP BY school;