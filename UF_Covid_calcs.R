
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


##############################################################
######## now join the archival data and scraped datasets
##############################################################

# First bind together all the daily scrapes into a single df
# path to folder that holds multiple .csv files
folder <- "./data_raw/daily_scrape/"      
scrape <- list.files(folder,   # Identify all csv files in folder
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                            # Store all files in list
  bind_rows %>%                                                     # Combine data sets into one data set 
  arrange(testing_program,group,value,update_date) %>% 
  select(-X1)
# data_all<-as.data.frame(data_all)
scrape


archive<-read_csv("./data_raw/googlesheet_import/UF_covid_data_20200818-20200907.csv") %>% 
  select(-X1) 
archive

corrected_date<-str_split(archive$update_date,"/")
corrected_date<-data.frame(matrix(unlist(corrected_date), nrow=length(corrected_date), byrow=T))
corrected_date$X3<-2020
corrected_date$X1<-paste(0,corrected_date$X1,sep="")
corrected_date$X2<-as.numeric(as.character(corrected_date$X2))
corrected_date <- corrected_date %>% 
  mutate(X2=ifelse(X2<10,paste(0,X2,sep=""),X2)) %>% 
  mutate(correct_date<-paste(X3,X1,X2,sep="-"))
corrected_date<-as.Date(corrected_date$correct_date)
archive$update_date<-corrected_date

# archive<-archive %>% filter(update_date<"2020-09-08") # choose whatever date you started scraping
# write.csv(scrape,paste("./data_raw/daily_scrape/UF_covid_data_2020-09-08.csv",sep=""))
# archive$update_date<-as.Date(archive$update_date,format("%Y %m %d"))
# archive$update_date<-as.Date(archive$update_date)

uf.data<-bind_rows(archive,scrape)

uf.data.SHC.s<-uf.data %>% 
  select(update_date,testing_program,group,value.2,N) %>% 
  filter(testing_program=="SHCC") %>%
  spread(value.2,N) %>% 
  select(-S.percpos.SHC) %>% 
  arrange(update_date) %>% 
  rename(Npos=S.pos.SHC,Ntest=S.test.SHC)


uf.data.RTC.fs<-uf.data %>% 
  select(update_date,testing_program,group,value.2,N) %>% 
  filter(testing_program=="RTC" & group=="fac.staff") %>%
  spread(value.2,N) %>% 
  select(-FS.percpos.RTC) %>% 
  arrange(update_date)%>% 
  rename(Npos=FS.pos.RTC,Ntest=FS.test.RTC)


uf.data.RTC.s<-uf.data %>% 
  select(update_date,testing_program,group,value.2,N) %>% 
  filter(testing_program=="RTC" & group=="students") %>%
  spread(value.2,N) %>% 
  arrange(update_date) %>% 
  select(-S.percpos.RTC) %>% 
  rename(Npos=S.pos.RTC,Ntest=S.test.RTC)


uf.data<-bind_rows(uf.data.RTC.s,uf.data.RTC.fs,uf.data.SHC.s) %>%
  group_by(testing_program,group) %>%
  mutate(new.tests=(Ntest-lag(Ntest)),
         new.pos=(Npos-lag(Npos)),
         daily.pos=new.pos/new.tests*100) %>%
  mutate(cmtv.perc.pos=Npos/Ntest*100) 
uf.data<-arrange(uf.data,group,update_date)



# Daily positivity rate with dashed lines for missing days
my_data<-uf.data %>%
  arrange(testing_program,group,update_date) %>% 
  group_by(testing_program,group) %>% 
  mutate(keep=ifelse(is.na(daily.pos)&lag(is.na(daily.pos)==TRUE),"del","keep")) %>% 
  arrange(testing_program,group,update_date) %>% 
  filter(keep=="keep")

gaps <- my_data %>%
  arrange(testing_program,group,update_date) %>% 
  group_by(testing_program,group) %>%
  filter(is.nan(lead(daily.pos)) & row_number() != n() |
           is.nan(lag(daily.pos)) & row_number() != 1) %>%
  mutate(gap.group = cumsum(row_number() %% 2)) 
 
UF.plot_daily<-ggplot(data = my_data, mapping = aes(x = update_date, y = daily.pos,color=paste(testing_program,group,sep=" "))) +
  geom_line() +
  geom_line(data = filter(gaps,group=="fac.staff" & testing_program=="RTC"), aes(group = gap.group), linetype = "dashed") +
  geom_line(data = filter(gaps,group=="students" & testing_program=="RTC"), aes(group = gap.group), linetype = "dashed") +
  geom_line(data = filter(gaps,group=="students" & testing_program=="SHCC"), aes(group = gap.group), linetype = "dashed") +
  geom_point() +
  labs(x="Date", y="Daily Positivity Rate (%)")+
  labs(title = "Daily Positivity Rate")
UF.plot_daily<-UF.plot_daily + theme_classic()+
  theme(legend.title=element_blank(),
        legend.position = c(0.1, 0.9))
UF.plot_daily

# Cumulative positivity rate
UF.plot_cumulative<-ggplot(data=uf.data, aes(x=update_date, y=cmtv.perc.pos, color=paste(testing_program,group,sep=" "))) +
  geom_line() +
  geom_point()+
  labs(x="Date", y="Cumulative Positivity Rate (%)")
UF.plot_cumulative<-UF.plot_cumulative + theme_classic()
UF.plot_cumulative



















# UF_covid_data$update_date<-as.Date(UF_covid_data$update_date)
# UF_covid_data$update_date<-format(UF_covid_data$update_date, format("%Y %m %d"))
raw.data<-read_sheet(ss="18AylEt8G7JF5LZ9A5QQJ3KjvYfr5ZBfZ8g7jFZ8eZ6A",
                     sheet="Raw data",
                     range = "C4:AH",
                     col_names = TRUE,
                     skip=3)
raw.data<-raw.data %>% select("date"="Updated",
                              "FS.test.RTC"="Faculty and Staff Tested (Without Symptoms)",
                              "FS.pos.RTC"="Faculty and Staff Positive (Without Symptoms)",
                              "FS.posperc.RTC"="Percent Positive...4",
                              "S.test.RTC"="Students Tested (Without Symptoms)",
                              "S.pos.RTC"="Students Positive (Without Symptoms)",
                              "S.posperc.RTC"="Percent Positive...7",
                              "S.test.SHC"="Students With or Without Symptoms Tested for COVID-19",
                              "S.pos.SHC"="Students With or Without Symptoms Confirmed as Positive",
                              "S.posperc.SHC"="Percent Positive...10",
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
  mutate(running.perc.pos=pos/tests*100) 
UF_data$group<-paste(UF_data$program,UF_data$category)
UF_data<-arrange(UF_data,group, date)


###################
# FIGURES
###################
testdata<-UF_data %>%
  arrange(date,group) %>% 
  group_by(group) %>% 
  filter(group=="SHCC (with or without symptoms) students")



# Daily positivity rate
UF.plot_daily<-ggplot(data=UF_data, aes(x=date, y=daily.pos, color=group)) +
  geom_line() +
  geom_point()+
  labs(x="Date", y="Daily Positivity Rate (%)")
UF.plot_daily<-UF.plot_daily + theme_classic()
UF.plot_daily


# Daily positivity rate with dashed lines for missing days
my_data<-UF_data %>%
  arrange(group,date) %>% 
  group_by(group) %>% 
  # filter(group=="SHCC (with or without symptoms) students") %>% 
  mutate(keep=ifelse(is.na(daily.pos)&lag(is.na(daily.pos)==TRUE),"del","keep")) %>% 
  arrange(group,date) %>% 
  filter(keep=="keep")

gaps <- my_data %>%
  arrange(date,group) %>% 
  group_by(group) %>%
  filter(is.nan(lead(daily.pos)) & row_number() != n() |
           is.nan(lag(daily.pos)) & row_number() != 1) %>%
  # This is needed to make a separate group for each pair of points.
  #  I expect it will break if a point ever has NA's on both sides...
  #  Anyone have a better idea?
  mutate(gap.group = cumsum(row_number() %% 2)) 
# 
# UF.plot_daily<-ggplot(data = my_data, mapping = aes(x = date, y = daily.pos,color=group)) +
#   geom_line() +
#   geom_line(data = filter(gaps,group=="SHCC (with or without symptoms) students"), aes(group = gap.group), linetype = "dashed") +
#   geom_line(data = filter(gaps,group=="RTC (without symptoms) Students"), aes(group = gap.group), linetype = "dashed") +
#   geom_line(data = filter(gaps,group=="RTC (without symptoms) Faculty & Staff"), aes(group = gap.group), linetype = "dashed") +
#   geom_point() +
#   labs(x="Date", y="Daily Positivity Rate (%)")+
#   labs(title = "Daily Positivity Rate")
# UF.plot_daily<-UF.plot_daily + theme_classic()
# UF.plot_daily

# Cumulative positivity rate
UF.plot_cumulative<-ggplot(data=UF_data, aes(x=date, y=running.perc.pos, color=group)) +
  geom_line() +
  geom_point()+
  labs(x="Date", y="Cumulative Positivity Rate (%)")
UF.plot_cumulative<-UF.plot_cumulative + theme_classic()
UF.plot_cumulative



