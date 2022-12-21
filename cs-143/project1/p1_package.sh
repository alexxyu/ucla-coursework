#!/usr/bin/env bash
ZIP_FILE="project1.zip"
REQUIRED_FILES="create.sql load.sql"
OPTIONAL_FILES="README.txt"

CUR_DIR=$(pwd -P)
SRC_DIR=${CUR_DIR}

#error function
function error_exit()
{
   echo -e "ERROR: $1" 1>&2
   rm -rf ${TMP_DIR}
   exit 1
}

# make sure running in container
if [ `whoami` != "cs143" ]; then
    error_exit "You need to run this script within the container"
fi

# if the source directory is passed as parameter, use it
if [ $# -eq 1 ]; then
    SRC_DIR=$1
fi

# remove the zip file if it already exists
if [ -f ${CUR_DIR}/${ZIP_FILE} ]; then
    rm -f ${CUR_DIR}/${ZIP_FILE}
fi

# change to the directory with the submission files
cd ${SRC_DIR}

# check the existence of the required files
for FILE in ${REQUIRED_FILES}
do
    if [ ! -f ${FILE} ]; then
        echo "ERROR: Cannot find ${FILE} in ${SRC_DIR}" 1>&2
        exit 1
    fi
done

# check the existence of the optional files
TO_ADD=""
for FILE in ${OPTIONAL_FILES}
do
    if [ -f ${FILE} ]; then
	TO_ADD+=" ${FILE}"
    fi
done

# check if load.sql loads data using relative path
LOAD_CMD=$(grep -E "\./movie.del" load.sql)
if [ -z "${LOAD_CMD}" ]; then
    error_exit 'The string "./movie.del" does not appear in load.sql. Are you using a RELATIVE path in your LOAD DATA command?'
fi

# create the zip file
echo "zip ${CUR_DIR}/${ZIP_FILE} ${REQUIRED_FILES} ${TO_ADD}"
zip ${CUR_DIR}/${ZIP_FILE} ${REQUIRED_FILES} ${TO_ADD}
if [ $? -ne 0 ]; then
    error_exit "Create ${CUR_DIR}/${ZIP_FILE} failed, check for error messages in console."
fi
echo "[SUCCESS] Created '${CUR_DIR}/${ZIP_FILE}'"
exit 0
