--In order for this script to run, you must first access sql by typing sqlite3, then enter .read PS3.sql
#/bin/sh
--We must first create the table the data will fit in to
CREATE TABLE FL_insurance_sample (
    Policy_ID INT PRIMARY KEY,
    State VARCHAR(2),
    County VARCHAR(100),
    TIV_2011 DECIMAL(15,2),
    TIV_2012 DECIMAL(15,2),
    TIV_2013 DECIMAL(15,2),
    TIV_2014 DECIMAL(15,2),
    TIV_2015 DECIMAL(15,2),
    TIV_2016 DECIMAL(15,2),
    Loss_Amount_2011 DECIMAL(15,2),
    Loss_Amount_2012 DECIMAL(15,2),
    Loss_Amount_2013 DECIMAL(15,2),
    Loss_Amount_2014 DECIMAL(15,2),
    Latitude DECIMAL(10,6),
    Longitude DECIMAL(10,6),
    Property_Use VARCHAR(50),
    Construction_Type VARCHAR(50),
    Flood_Zone INT
);
--we will now import the csv data into our table
.mode csv
.import FL_insurance_sample.csv FL_insurance_sample
--Viewing the first 10 rows of our table
SELECT * FROM FL_insurance_sample LIMIT 10;
--This prints all unique values of the county variable
SELECT DISTINCT County FROM FL_insurance_sample;
--tiv_2012 - tiv_2011 creates the appreciation value, which we then find the average of
SELECT AVG(tiv_2012 - tiv_2011) FROM FL_insurance_sample;
--This displays the number of times each construction type appears as well as the proportion of the sample that is each construction type value
SELECT 
    Construction_Type, 
    COUNT(*) AS Frequency,
    COUNT(*) * 1.0 / (SELECT COUNT(*) FROM FL_insurance_sample) AS Fraction
FROM FL_insurance_sample
GROUP BY Construction_Type;

