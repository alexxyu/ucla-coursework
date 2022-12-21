SELECT dept, 
CASE
    WHEN dept = 'Comp. Sci.' OR dept = 'Elec. Eng.' THEN 'Engineering'
    ELSE 'L&S'
END school
FROM Department;