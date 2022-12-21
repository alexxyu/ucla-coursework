SELECT S.name student_name, I.name advisor_name
FROM Student S
LEFT OUTER JOIN Advisor A
ON S.id = A.stud_id
LEFT OUTER JOIN Instructor I
ON A.inst_id = I.id;