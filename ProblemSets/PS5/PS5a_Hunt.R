#Scraping Economics Research Rankings from RePEc
#calling rvest
library(rvest)

#Getting website url
url <- "https://ideas.repec.org/top/top.usecondept.html"

# Read the HTML of the page
page <- read_html(url)

#Extracting table using css selector copied from the websites table element
table <- page %>% html_node(css = "#ranking > table") %>% 
  html_table(fill = TRUE)

#converting to a dataframe object
df <- as.data.frame(table)
print(df)