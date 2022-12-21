SELECT dept, MAX(credits) maximum_course_credit
FROM Class
GROUP BY dept;