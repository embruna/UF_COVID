UF_data_bind<-function(blank) {
##############################3
# Script to load the daily scraped files, join them together, 
# join them sith some data collected prior to webscraping,
# and graph
library(tidyverse)

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
scrape$data_source<-"scrape"

############################################################
# read in some data collected before starting to scrape
############################################################

archive<-read_csv("./data_raw/googlesheet_import/UF_covid_data_20200818-20200907.csv") %>% 
  select(-X1) 
archive

missing.data<-read_csv("./data_raw/missing.data.dates.csv")

archive<-bind_rows(archive,missing.data)

corrected_date<-str_split(archive$update_date,"/")
corrected_date<-data.frame(matrix(unlist(corrected_date), nrow=length(corrected_date), byrow=T))
corrected_date$X3<-2020
corrected_date$X1<-paste(0,corrected_date$X1,sep="")
corrected_date$X2<-as.numeric(as.character(corrected_date$X2))
corrected_date <- corrected_date %>% 
  mutate(X2=ifelse(X2<10,paste(0,X2,sep=""),X2)) %>% 
  mutate(correct_date=paste(X3,X1,X2,sep="-"))
corrected_date<-as.Date(corrected_date$correct_date)
archive$update_date<-corrected_date
archive$data_source<-"ggsheet"

############################################################
# read in some data collected before starting to scrape
############################################################

missing_days<-read_csv("./data_raw/SKME.csv")
corrected_date<-str_split(missing_days$scrape_date,"/")
corrected_date<-data.frame(matrix(unlist(corrected_date), nrow=length(corrected_date), byrow=T))
corrected_date$X3<-2020
corrected_date$X1<-paste(0,corrected_date$X1,sep="")
corrected_date$X2<-as.numeric(as.character(corrected_date$X2))
corrected_date <- corrected_date %>% 
  mutate(X2=ifelse(X2<10,paste(0,X2,sep=""),X2)) %>% 
  mutate(correct_date=paste(X3,X1,X2,sep="-"))
missing_days$scrape_date<-corrected_date$correct_date
missing_days$update_date<-corrected_date$correct_date
missing_days$scrape_date<-as.Date(missing_days$scrape_date)
missing_days$update_date<-as.Date(missing_days$update_date)
missing_days$data_source<-"sk"

# these dates are missing from the pre-scrape datasheet



############################################################
# bind them up, split by group, and clean up titles, etc. 
############################################################
uf.data<-bind_rows(archive,scrape,missing_days)



############################################################
# Put it all back together, get the number of new tests 
# and new postive tests per day, and calclulate the daily
# and cumulative positive test %
############################################################

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

uf.totals<-uf.data %>%
  group_by(update_date) %>% 
  summarize(Npos=sum(Npos),
            Ntest=sum(Ntest),
            new.tests=sum(new.tests),
            new.pos=sum(new.pos)) %>% 
  mutate(daily.pos=new.pos/new.tests*100) %>%
  mutate(cmtv.perc.pos=cumsum(daily.pos))
  
uf.totals$group<-"UF Total"
uf.totals$testing_program<-"SHCC+RTC"
uf.totals$cat<-"UF Total"
uf.data<-bind_rows(uf.data,uf.totals)

uf.data<-uf.data %>%
  mutate(cat=ifelse(testing_program=="SHCC","Students\n(SHCC)",cat)) %>% 
  mutate(cat=ifelse(group=="students" & testing_program=="RTC","Students\n(RTC)",cat)) %>% 
  mutate(cat=ifelse(group=="fac.staff" & testing_program=="RTC","Fac & Staff\n(RTC)",cat))  
uf.data$group <- as.factor(uf.data$group)
uf.data$testing_program <- as.factor(uf.data$testing_program)
# uf.data$cat <- as.factor(uf.data$cat)

#save as a csv file with the date 
write_csv(uf.data,paste("./data_clean/UFcovid_data_",Sys.Date(),".csv",sep=""))
return(uf.data)
}