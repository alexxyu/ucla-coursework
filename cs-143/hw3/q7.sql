SELECT C1.dept, dept_avg_course_credit, overall_avg_course_credit
FROM 
    (SELECT dept, AVG(credits) dept_avg_course_credit FROM Class C GROUP BY dept) C1,
    (SELECT DISTINCT dept, AVG(credits) OVER() overall_avg_course_credit FROM Class C) C2
WHERE C1.dept = C2.dept;