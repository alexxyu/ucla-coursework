SELECT dept, (SELECT AVG(salary) FROM Instructor)-AVG(salary) diff_avg_salary
FROM Instructor
GROUP BY dept;