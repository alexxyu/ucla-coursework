# CS 143

## Docker Containers
To set up one of the Docker containers, run a command of the following form:

`$ docker run -it -v {your_shared_dir}:/home/cs143/shared -p 8888:80 --name {container_name} {image_name}`

| Container Name | Image Name             | Start Command                  | Project |
| -------------- | ---------------------- | ------------------------------ | ------- |
| mysql-apache   | `junghoo/mysql-apache` | `docker start -i mysql-apache` | 1, 2, 3 |
| mongo-apache   | `junghoo/mongo-apache` | `docker start -i mongo-apache` | 4       |
| spark          | `junghoo/spark`        | `docker start -i spark`        | 5       |
| unix           | `junghoo/unix`         | `docker start -i unix`         | 6       |

## Miscellaneous Setup
Project 6 uses Google Books N-gram data for data processing. The [1-gram-s](http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-1gram-20120701-s.gz) file should be placed in the `/home/cs143/data` directory. The file is not included in this repository due to size.
