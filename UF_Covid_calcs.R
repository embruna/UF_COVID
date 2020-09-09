library(googlesheets4)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
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
UF_covid_data<-bind_cols(scrape_date=Sys.Date(),
                         scrape_time=format(Sys.time(), "%X"),
                         update_date=update_date,
                         cats,
                         value.2=value.2,
                         numbers)

write.csv(UF_covid_data,paste("./daily_scrape/UF_covid_data",Sys.Date(),".csv"))



##############################################################################
# this is the old version to scrape off the google sheet set up by reddit user 
# might still be useful to fill in missing days
##############################################################################
raw.data<-read_sheet(ss="18AylEt8G7JF5LZ9A5QQJ3KjvYfr5ZBfZ8g7jFZ8eZ6A",
                sheet="Raw data",
                range = "C4:AH",
                col_names = TRUE,
                skip=3)

raw.data %>% gather(Category, Count, raw.data[2]:raw.data[14])
DF %>% gather(Quarter, Revenue, Qtr.1:Qtr.4)

gather(raw.data,Updated,category)
raw.data<-raw.data %>% select("date"="Updated",
                              "FS.test.RTC"="Faculty and Staff Tested (Without Symptoms)",
                              "FS.pos.RTC"="Faculty and Staff Positive (Without Symptoms)",
                              "S.test.RTC"="Students Tested (Without Symptoms)",
                              "S.pos.RTC"="Students Positive (Without Symptoms)",
                              "S.test.SHC"="Students With or Without Symptoms Tested for COVID-19",
                              "S.pos.SHC"="Students With or Without Symptoms Confirmed as Positive",
                              "UF.affil.cases"="Cases Identified and Followed Among UF Affiliates",
                              "contacts.followed"="Contacts of UF Affiliates Identified and Followed",
                              "UF.isol"="UF Affiliates or Contacts of UF Affiliates Currently in Isolation or Quarantine",
                              "UF.affil.back"="UF Affiliates Returned to Campus")


##########################
# SHCC Data
##########################
SHCC<-raw.data %>%
  select(date,tests=S.test.SHC,pos=S.pos.SHC)
SHCC$program<-"SHCC (with or without symptoms)"
SHCC$category<-"students"

##########################
# RTC Data
##########################

RTC.S<-raw.data %>%
  select(date,tests=S.test.RTC,pos=S.pos.RTC)
RTC.S$program<-"RTC (without symptoms)"
RTC.S$category<-"Students"


RTC.FS<-raw.data %>%
  select(date,tests=FS.test.RTC,pos=FS.pos.RTC)
RTC.FS$program<-"RTC (without symptoms)"
RTC.FS$category<-"Faculty & Staff"

RTC<-bind_rows(RTC.FS,RTC.S)

########################################
# Bind SHCC and RTC data together
########################################

UF_data<-bind_rows(RTC,SHCC) %>%
  group_by(program,category) %>%
mutate(new.tests=(tests-lag(tests)),
       new.pos=(pos-lag(pos)),
       daily.pos=new.pos/new.tests*100) %>%
  mutate(running.perc.pos=pos/tests*100) %>%
UF_data$group<-paste(UF_data$program,UF_data$category)
UF_data<-arrange(UF_data,group, date)


###################
# FIGURES
###################

# Daily positivity rate
UF.plot_daily<-ggplot(data=UF_data, aes(x=date, y=daily.pos, color=group)) +
  geom_line() +
  geom_point()+
  labs(x="Date", y="Daily Positivity Rate (%)")
UF.plot_daily<-UF.plot_daily + theme_classic()
UF.plot_daily

# Cumulative positivity rate
UF.plot_cumulative<-ggplot(data=UF_data, aes(x=date, y=running.perc.pos, color=group)) +
  geom_line() +
  geom_point()+
  labs(x="Date", y="Cumulative Positivity Rate (%)")
UF.plot_cumulative<-UF.plot_cumulative + theme_classic()
UF.plot_cumulative



