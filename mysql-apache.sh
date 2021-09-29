#!/bin/zsh

dir=$(pwd)
#docker run -it -v $dir:/home/cs143/shared -p 8888:80 junghoo/mysql-apache
docker start -i mysql-apache
