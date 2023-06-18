# About Data base connections 

# Mainly look at DBI R package

# For others check it out; 
# https://cran.r-project.org/web/views/Databases.html


# SQL example -------------------------------------------------------------

# SQLDF package -----------------------------------------------------------

install.packages("sqldf")
# asking to install blob and RSQLite as well  
library(sqldf)

# Some data set available
data(EuStockMarkets)
head(EuStockMarkets)

# Consider it as data frame
EuStock <- as.data.frame(EuStockMarkets)
class(EuStock)

# choosing only one stock 
sqldf('SELECT DAX FROM EuStock')

# choose also a subset 
sqldf('SELECT DAX FROM EuStock LIMIT 5')
head(EuStock$DAX, 5)

# put a condition 
sqldf("SELECT * FROM EuStock WHERE DAX > 1620")

# Wild card to select all
sqldf('SELECT * FROM EuStock')
sqldf('SELECT * FROM EuStock LIMIT 5')

# Ordering 
sqldf('SELECT * FROM EuStock ORDER BY CAC ASC LIMIT 5')

# Aggregated data
sqldf("SELECT AVG(DAX) FROM EuStock")


# RSQLite -----------------------------------------------------------------


# RSQLite
# install.packages("RSQLite")
library(RSQLite)

# To create a new SQLite database

mydb <- dbConnect(RSQLite::SQLite(), "my-dbsql")
dbDisconnect(mydb)
unlink("my-dbsql")

# For a temporary database it can be used the following
# mydb <- dbConnect(RSQLite::SQLite(), "")
# dbDisconnect(mydb)

# Copying a data set into new database
mydb <- dbConnect(RSQLite::SQLite(), "my-dbsql")
dbWriteTable(mydb, "EuStock", EuStock)
dbListTables(mydb)

# Applying simple query with dbGetQuery():
# dbGetQuery(mydb, 'SELECT * FROM mtcars LIMIT 5')
dbGetQuery(mydb, 'SELECT * FROM EuStock LIMIT 10')

# Some queries

dbGetQuery(mydb, "SELECT * FROM EuStock WHERE DAX > 1620 LIMIT 10")

# If you need to insert the value from a user into a query, donât use paste()! 
# That makes it easy for a malicious attacker to insert SQL that might damage your 
# database or reveal sensitive information. Instead, use a parameterised query:

x <- 1660
dbGetQuery(mydb, 'SELECT * FROM EuStock WHERE "DAX" > :x LIMIT 10', params = list(x = x))


# dbplyr ------------------------------------------------------------------


# Introduction to dbplyr
library(dplyr)
library(dbplyr)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
# with :memory:, creating a temporary in-memory database.


# Copying data to con 

copy_to(con, EuStock, "EuStock",
        temporary = FALSE, 
        indexes = list("DAX", "SMI", "CAC", "FTSE") )

# Put a reference on it 
EuStock_db <- tbl(con, "EuStock")
EuStock_db
class(EuStock_db)

# Some queries like dplyr syntax
EuStock_db %>% select(DAX)

EuStock_db %>% filter(DAX > 1620) 


EuStock_db %>% 
  select(DAX, SMI) %>% 
  filter(DAX > 1620) %>% 
  head()


# Harvesting with rvest ---------------------------------------------------

# Data collection via web scraping 

# Load {rvest} with the whole tidyverse
library(tidyverse)
library(magrittr)
library(rvest)
library(gt)

# Read HTML page with read_html()
link <- 'http://www.turkmath.org/beta/webdergiler.php'
# Create html document from an url

journals_TR <- read_html(link)
class(journals_TR)
# Open the borsa_IST object
# Language is TR, we see the head and body part here
journals_TR
class(journals_TR)

# Regardless of how you get the HTML, youâll need some way to identify 
# the elements that contain the data you care about. 
# rvest provides two options: CSS selectors and XPath expressions. 
# CSS selectors are simpler but still sufficiently powerful for most scraping tasks

# Reaching to the table !
Journal_List <-  journals_TR %>% 
  rvest::html_element(".table") %>% 
  rvest::html_table()

class(Journal_List)
# More steps to format this table
attach(Journal_List)

SCI_List <- Journal_List %>% 
  # Rename first column
  rename(Index=1) %>% 
  # Filtering only TR Dizin
  filter(Index == "TR Dizin") %>% 
  # Among those, filter only SCI column is not empty
  filter(SCI == "ESCI" | SCI == "SCIexp")


# Some further readings

# https://cran.r-project.org/web/packages/DBI/vignettes/DBI.html

# https://medium.com/@ozanevkaya/ve-r-i-tabanlar%C4%B1-ile-k%C4%B1sa-konu%C5%9Fmalar-33c2c890c939

# https://medium.com/@ozanevkaya/elleriyle-kazanlar-veri-kaz%C4%B1man%C4%B1n-g%C3%BCc%C3%BC-ad%C4%B1na-8d9315cbc629

# https://medium.com/@ozanevkaya/r-paketlerinde-bug%C3%BCn-datapasta-316fc40c8feb


