UFdaily_plot<-function() {

library(tidyverse)
UF_daily<-read_csv("./data_raw/UF_cases_per_day.csv")

corrected_date<-str_split(UF_daily$update_date,"/")
corrected_date<-data.frame(matrix(unlist(corrected_date), nrow=length(corrected_date), byrow=T))
corrected_date$X3<-2020
corrected_date$X1<-paste(0,corrected_date$X1,sep="")
corrected_date$X2<-as.numeric(as.character(corrected_date$X2))
corrected_date <- corrected_date %>% 
  mutate(X2=ifelse(X2<10,paste(0,X2,sep=""),X2)) %>% 
  mutate(correct_date=paste(X3,X1,X2,sep="-"))
UF_daily$update_date<-corrected_date$correct_date
UF_daily$update_date<-as.Date(UF_daily$update_date)


UF_daily<-UF_daily %>% 
  mutate(cumulative=cumsum(cases))


# The plot
UFdaily_plot <- ggplot(UF_daily, aes(update_date, cases)) +
  ggtitle("UF-affiliated cases identified each day") +
  xlab("Date of Disease Onset") + ylab("Number of Cases") +
  geom_bar(stat="identity", colour="#0021A5",fill="#0021A5")+
  # geom_line(aes(x=update_date,y=cumulative, color="#FA4616"),size=1.5) +
  scale_x_date(date_breaks = "15 day", date_labels = "%b %d",expand = c(0, .9))


UFdaily_plot <- UFdaily_plot + theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10, color="#0021A5"),
    axis.title.x = element_text(colour = "black", size = 16),
    axis.title.y = element_text(colour = "#0021A5", size = 16),
    plot.margin =unit(c(1,1,1,1), "lines"),  
    # legend.text = element_text(colour = "black", size = 10, vjust = 0.5),
    plot.title = element_text(size = 24,hjust = 0.5),
    # plot.subtitle = element_text(size = 14,hjust = 0.5),
  )
return(UFdaily_plot)
}

# sum(UF_daily$cases)