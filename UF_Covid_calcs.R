library(googlesheets4)
library(tidyr)
library(dplyr)
library(ggplot2)


raw.data<-read_sheet(ss="18AylEt8G7JF5LZ9A5QQJ3KjvYfr5ZBfZ8g7jFZ8eZ6A",
                sheet="Raw data",
                range = "C4:AH",
                col_names = TRUE,
                skip=3)

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
                              "UF.affil.back"="UF Affiliates Returned to Campus") %>% 
  # This is daily positivity rates ()
  mutate(new.S.test.SHC=(S.test.SHC-lag(S.test.SHC)),
         new.S.pos.SHC=(S.pos.SHC-lag(S.pos.SHC)),
         daily.S.pos.SHC=new.S.pos.SHC/new.S.test.SHC*100) %>% 
  #these are running percent positivity- from day 1, cumulative, through day N
  mutate(running.FS.RTC.perc.pos=FS.pos.RTC/FS.test.RTC*100,   
         running.S.RTC.perc.post=S.pos.RTC/S.test.RTC*100,
         running.S.SHC.perc.pos=S.pos.SHC/S.test.SHC*100)

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



