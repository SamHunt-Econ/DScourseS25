#/bin/sh
wget http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip
ls
unzip FL_insurance_sample.csv.zip
rm -rf __MAC0SX
rm -f FL_insurance_sample.csv.zip
-al --block-size=MB FL_insurance_sample.csv
head -5 FL_insurance_sample.csv
wc -l FL_insurance_sample.csv
dos2unix -c mac FL_insurance_sample.csv
head -5 FL_insurance_sample.csv
wc -l FL_insurance_sample.csv

