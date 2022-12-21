SELECT dept
FROM Department
WHERE dept NOT IN (
    SELECT dept
    FROM Class
    WHERE credits != 3
);