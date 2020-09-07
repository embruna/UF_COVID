library(googlesheets4)
library(tidyr)
library(dplyr)
library(ggplot2)


raw.data<-read_sheet(ss="18AylEt8G7JF5LZ9A5QQJ3KjvYfr5ZBfZ8g7jFZ8eZ6A",
                sheet="Raw data",
                range = "C4:AH17",
                col_names = TRUE,
                skip=3)

raw.data<-raw.data %>% select("date"="Updated",
                              "FS.test"="Faculty and Staff Tested (Without Symptoms)",
                              "FS.pos"="Faculty and Staff Positive (Without Symptoms)",
                              "S.test"="Students Tested (Without Symptoms)",
                              "S.pos"="Students Positive (Without Symptoms)",
                              "S.test.SHC"="Students With or Without Symptoms Tested for COVID-19",	
                              "S.pos.SHC"="Students With or Without Symptoms Confirmed as Positive",	
                              "UF.affil.cases"="Cases Identified and Followed Among UF Affiliates",
                              "contacts.followed"="Contacts of UF Affiliates Identified and Followed",
                              "UF.isol"="UF Affiliates or Contacts of UF Affiliates Currently in Isolation or Quarantine",
                              "UF.affil.back"="UF Affiliates Returned to Campus") %>% 
  mutate(FS.RTC.perc.pos=FS.pos/FS.test*100,
         S.RTC.perc.post=S.pos/S.test*100,
         S.SHC.perc.pos=S.pos.SHC/S.test.SHC*100)



ggplot(data=raw.data, aes(x=date, y=S.SHC.perc.pos)) +
  geom_line() +
  geom_point()+
  labs(x="Date", y="Percent Positive (Students, SHCC)")
  


