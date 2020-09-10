
##############################################################################
# to import data from google sheet createtd by redddit user 'data_dude`
##############################################################################

library(tidyverse)
library(googlesheets4)
gsheet.data<-read_sheet(ss="18AylEt8G7JF5LZ9A5QQJ3KjvYfr5ZBfZ8g7jFZ8eZ6A",
                     sheet="Raw data",
                     range = "C4:AH",
                     col_names = TRUE,
                     skip=3)

colnames(gsheet.data)

gsheet.data<-gsheet.data %>% select("update_date"="Updated",
                              "FS.test.RTC"="Faculty and Staff Tested (Without Symptoms)",
                              "FS.pos.RTC"="Faculty and Staff Positive (Without Symptoms)",
                              "FS.percpos.RTC"="Percent Positive...4",
                              "S.test.RTC"="Students Tested (Without Symptoms)",
                              "S.pos.RTC"="Students Positive (Without Symptoms)",
                              "S.percpos.RTC"="Percent Positive...7",
                              "S.test.SHC"="Students With or Without Symptoms Tested for COVID-19",
                              "S.pos.SHC"="Students With or Without Symptoms Confirmed as Positive",
                              "S.percpos.SHC"="Percent Positive...10",
                              "UF.affil.cases"="Cases Identified and Followed Among UF Affiliates",
                              "contacts.followed"="Contacts of UF Affiliates Identified and Followed",
                              "UF.isol"="UF Affiliates or Contacts of UF Affiliates Currently in Isolation or Quarantine",
                              "UF.affil.back"="UF Affiliates Returned to Campus") %>% 
  gather(value.2, N, 'FS.test.RTC':'UF.affil.back') %>% 
  arrange(update_date) 
a<-levels(as.factor(gsheet.data$value.2))
b<-c("Contacts of UF Affiliates Identified and Followed",
     "Faculty and Staff Positive (Without Symptoms)",
     "Percent Positive",
     "Faculty and Staff Tested (Without Symptoms)",
     "Students Positive (Without Symptoms)",
     "Students With or Without Symptoms Confirmed as Positive",
     "Percent Positive",
     "Percent Positive",
     "Students Tested (Without Symptoms)",
     "Students With or Without Symptoms Tested for COVID-19",
     "UF Affiliates Returned to Campus",
     "Cases Identified and Followed Among UF Affiliates", 
     "UF Affiliates or Contacts of UF Affiliates Currently in Isolation or Quarantine")
ab<-bind_cols(a,b)
names(ab)<-c("value.2","value")
ab<-ab %>% 
  arrange(value.2) %>% 
  bind_cols(testing_program=c("contact.tracing", 
                              rep("RTC",4), 
                              rep(c("SHCC","RTC"),2),
                              "SHCC",
                              rep("contact.tracing",3))) %>% 
  arrange(testing_program) %>% 
  bind_cols(group=c(rep("uf.affil",4),
                    rep("fac.staff",3), 
                    rep("students",6)))

gsheet.data<-left_join(gsheet.data,ab) %>% 
  arrange(testing_program,value,update_date)
rm(a,b,ab)

gsheet.data$N<-round(gsheet.data$N,2)
gsheet.data$update_date<-as.Date(gsheet.data$update_date,format("%Y %m %d"))

write.csv(gsheet.data,paste("./data_raw/daily_scrape/UF_covid_data_",Sys.Date(),".csv", sep=""))

