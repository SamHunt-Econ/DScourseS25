#!/usr/bin/env Rscript
#loading the file
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

#view the file
system('cat dates.json')

#load packages
install.packages("jsonlite", repos='http://cran.us.r-project.org')
install.packages("tidyverse", repos='http://cran.us.r-project.org')
library(jsonlite)
library(tidyverse)

#converting JSON to dataframe
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

#determining the object type of mydf$date
class(mydf$date)

#listing the first n rows of mydf, letting n = 10
head(mydf, n = 10)
