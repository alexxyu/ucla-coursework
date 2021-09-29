CREATE TABLE Movie(id int not null, title varchar(100), year int, rating varchar(10), company varchar(50), PRIMARY KEY(id));
CREATE TABLE Actor(id int not null, last varchar(20), first varchar(20), sex varchar(6), dob date, dod date, PRIMARY KEY(id));
CREATE TABLE MovieGenre(mid int, genre varchar(20));
CREATE TABLE MovieActor(mid int, aid int, role varchar(50));
CREATE TABLE Review(name varchar(20), time datetime, mid int, rating int, comment text);
