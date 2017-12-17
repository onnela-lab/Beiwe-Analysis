#!/bin/bash
export ZIPFILE=study-data.zip
export DIR=unzipped-data
export INFOFILE=zipinfo.txt

echo 0
export FILE=$(zipinfo -1 ${ZIPFILE} | head -n +3 | tail -n -1)
echo ${FILE}
echo 1
unzip ${ZIPFILE} "${FILE}" -d ${DIR}
echo 2
zipinfo ${ZIPFILE} | head -n +12 > ${DIR}/${INFOFILE}
echo 3
ls ${DIR}
echo 4
python3 Beiwe-Analysis/test.py
echo 5
zip -r upload-data.zip ${DIR}
echo 6
python3 upload_s3_files.py upload-data.zip
echo 7
