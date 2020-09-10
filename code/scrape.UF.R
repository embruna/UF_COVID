####################################################################################
# Script to scrape data from UF Covid webiste and save day's results as a csv file
####################################################################################
library(tidyverse)
library(rvest)

UF_covid_page <- "https://coronavirus.ufhealth.org/screen-test-protect-2/about-initiative/testing-dashboard"
UF_covid <- read_html(UF_covid_page)
UF_covid
str(UF_covid)

body_nodes <- UF_covid %>% 
  html_node("body") %>% 
  html_children()
body_nodes %>% html_children()
xml_find_all(UF_covid)

numbers<-UF_covid %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'd-ib py-8 px-16')]") %>% 
  rvest::html_text()
numbers<-gsub("\n","",numbers)
numbers<-gsub(",","",numbers)
numbers<-gsub("%","",numbers)
numbers<-trimws(numbers)
numbers<-as_tibble(numbers)
colnames(numbers)<-"N"


update_date<-UF_covid %>% 
  html_node("head") %>% 
  html_children()
update_date<-update_date[8]
update_date<-as.character(update_date)
update_date<-as_tibble(update_date) 
update_date<-sub(".*Last updated ", "",update_date) %>% 
  str_split(". ")
update_date<-update_date[[1]][1]


cats<-UF_covid %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'd-b fs-14 fs-16--lg')]") %>% 
  rvest::html_text()
cats<-gsub("\n","",cats)
cats<-trimws(cats)
cats<-as_tibble(cats)
colnames(cats)<-"value"
cats<-bind_cols(testing_program=c(rep("RTC",6),rep("SHCC",3),rep("contact.tracing",4)),
                group=c(rep("fac.staff",3),rep("students",6),rep("uf.affil",4)),
                cats)

value.2<-c("FS.test.RTC","FS.pos.RTC","FS.percpos.RTC",
           "S.test.RTC","S.pos.RTC","S.percpos.RTC",
           "S.test.SHC","S.pos.SHC","S.percpos.SHC",
           "UF.affil.cases","contacts.followed","UF.isol","UF.affil.back")

# UF_covid_data$date<-Sys.Date()
# UF_covid_data$time<-format(Sys.time(), "%X")
UF_web_data<-bind_cols(scrape_date=Sys.Date(),
                       scrape_time=format(Sys.time(), "%X"),
                       update_date=update_date,
                       cats,
                       value.2=value.2,
                       numbers)

# Cobnvert counts/percs from character to numeric from 
UF_web_data$N<-as.numeric(UF_web_data$N)



# convert date to date and put in proper format
corrected_date<-str_split(UF_web_data$update_date,"/")
corrected_date<-data.frame(matrix(unlist(corrected_date), nrow=length(corrected_date), byrow=T))
corrected_date$X3<-2020
corrected_date$X1<-paste(0,corrected_date$X1,sep="")
corrected_date$X2<-as.numeric(as.character(corrected_date$X2))
corrected_date <- corrected_date %>% 
  mutate(X2=ifelse(X2<10,paste(0,X2,sep=""),X2)) %>% 
  mutate(correct_date=paste(X3,X1,X2,sep="-"))
corrected_date<-as.Date(corrected_date$correct_date)
UF_web_data$update_date<-corrected_date

# Save as a CSV
write.csv(UF_web_data,paste("./data_raw/daily_scrape/UF_covid_data_",Sys.Date(),".csv",sep=""))
