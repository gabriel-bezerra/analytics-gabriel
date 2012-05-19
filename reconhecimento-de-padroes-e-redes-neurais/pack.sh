#!/bin/sh

LAB=$1
LABXX=$(echo $LAB | tr '[:lower:]' '[:upper:]') # to upper case
NOME=GABRIEL
OUTPUT=gabriel-PRNN-$LABXX.zip

if [ ! -d $LAB/resposta ]; then
    echo $LAB must be a directory with a directory resposta inside
    exit -1
fi

# ./
echo Making temporary directory
mkdir tmp
cd tmp

# ./tmp
echo Making directory: $LABXX
mkdir $LABXX
cd $LABXX

# ./tmp/$LABXX
echo Making directory: $LABXX/$NOME
ln -s ../../$LAB/resposta $NOME
cd $NOME

# ./tmp/$LABXX/$NOME
echo Converting ODTs to PDF
libreoffice --headless --convert-to pdf:writer_pdf_Export *.odt
cd ../..

# ./tmp/
echo Creating package: $OUTPUT with all the '*.{R,txt,pdf,csv,dat}' files
zip $OUTPUT $LABXX/$NOME/*.{R,txt,pdf,csv,dat}
mv $OUTPUT ..
cd ..

# ./
echo Cleaning up
rm -r tmp

echo Finished

