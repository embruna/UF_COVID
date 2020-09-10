##############################3
# Script to load the daily scraped files, join them together, 
# join them sith some data collected prior to webscraping,
# and graph
library(tidyverse)
library(rvest)
library(RColorBrewer)

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
# one<-as.data.frame(scrape[1])
# one<-one %>% select(-X1) 
# corrected_date<-str_split(one$scrape_date,"/")
# corrected_date<-data.frame(matrix(unlist(corrected_date), nrow=length(corrected_date), byrow=T))
# corrected_date$X3<-2020
# corrected_date$X1<-paste(0,corrected_date$X1,sep="")
# corrected_date$X2<-as.numeric(as.character(corrected_date$X2))
# corrected_date <- corrected_date %>% 
#   mutate(X2=ifelse(X2<10,paste(0,X2,sep=""),X2)) %>% 
#   mutate(correct_date=paste(X3,X1,X2,sep="-"))
# corrected_date<-as.Date(corrected_date$correct_date)
# one$scrape_date<-corrected_date
# write_csv(one,"./data_raw/daily_scrape/UF_covid_data_2020-09-08.csv")

############################################################
# read in some data collected before starting to scrape
############################################################

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
  mutate(correct_date=paste(X3,X1,X2,sep="-"))
corrected_date<-as.Date(corrected_date$correct_date)
archive$update_date<-corrected_date

############################################################
# bind them up, split by group, and clean up titles, etc. 
############################################################
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

############################################################
# Put it all back together, get the number of new tests 
# and new postive tests per day, and calclulate the daily
# and cumulative positive test %
############################################################

uf.data<-bind_rows(uf.data.RTC.s,uf.data.RTC.fs,uf.data.SHC.s) %>%
  group_by(testing_program,group) %>%
  mutate(new.tests=(Ntest-lag(Ntest)),
         new.pos=(Npos-lag(Npos)),
         daily.pos=new.pos/new.tests*100) %>%
  mutate(cmtv.perc.pos=Npos/Ntest*100) 
uf.data<-arrange(uf.data,group,update_date)


############################################################
# GRAPHS
############################################################

# becauser there are some days with no data (weekends, etc)
# there will be breaks in the line. This patches the breaks with a 
# dashed line
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
 

######################################################
# PLOT OF percent of each days tests that are positive
######################################################

UF.plot_daily <- ggplot(
  data = my_data,
  mapping = aes(
    x = update_date,
    y = daily.pos,
    color = paste(testing_program, group, sep = " "))) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  # scale_x_date(date_minor_breaks = "3 day")+
  geom_line() +
  geom_line(data = filter(gaps, group == "fac.staff" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
  geom_line(data = filter(gaps, group == "students" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
  geom_line(data = filter(gaps, group == "students" & testing_program == "SHCC"), aes(group = gap.group), linetype = "dashed") +
  geom_point() +
  scale_shape_manual(values = c(21, 21, 21)) +
  scale_color_brewer(palette = "Dark2") +
  geom_text(aes(label = round(daily.pos, 1)), hjust = 0, vjust = -1, show.legend = FALSE) +
  labs(x = "Date", y = "Percent") +
  labs(title = "UF - Positive Tests Per Day (%)")
UF.plot_daily <- UF.plot_daily + theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2, 0.9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(colour = "black", size = 16),
    axis.title.y = element_text(colour = "black", size = 16),
    plot.title = element_text(size = 20),
    legend.text = element_text(colour = "black", size = 12, vjust = 0.5)
  )
UF.plot_daily




