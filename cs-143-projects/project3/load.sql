DROP TABLE IF EXISTS Person;
DROP TABLE IF EXISTS Organization;
DROP TABLE IF EXISTS Prize;
DROP TABLE IF EXISTS Affiliation;

CREATE TABLE Person(
    id INT PRIMARY KEY, 
    given_name VARCHAR(50), 
    family_name VARCHAR(50), 
    gender VARCHAR(6), 
    birth_date DATE, 
    birth_city VARCHAR(30), 
    birth_country VARCHAR(30)
);

CREATE TABLE Organization(
    id INT PRIMARY KEY, 
    org_name VARCHAR(100), 
    founded_date DATE, 
    founded_city VARCHAR(30), 
    founded_country VARCHAR(30)
);

CREATE TABLE Prize(
    prize_id INT PRIMARY KEY, 
    winner_id INT, 
    year INT,
    category VARCHAR(22),
    sort_order INT
);

CREATE TABLE Affiliation(
    prize_id INT PRIMARY KEY REFERENCES Prize,
    affiliation_name VARCHAR(100),
    city VARCHAR(30),
    country VARCHAR(30)
);

LOAD DATA LOCAL INFILE './Person.del' INTO TABLE Person;
LOAD DATA LOCAL INFILE './Organization.del' INTO TABLE Organization;
LOAD DATA LOCAL INFILE './Prize.del' INTO TABLE Prize;
LOAD DATA LOCAL INFILE './Affiliation.del' INTO TABLE Affiliation;