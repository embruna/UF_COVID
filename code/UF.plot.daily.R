UF.plot.daily<-function(uf.data) {

# becauser there are some days with no data (weekends, etc)
# there will be breaks in the line. This patches the breaks with a 
# dashed line
my_data<-uf.data %>%
  arrange(testing_program,group,update_date) %>% 
  group_by(testing_program,group) %>% 
  mutate(keep=ifelse(is.na(daily.pos)&lag(is.na(daily.pos)==TRUE),"del","keep")) %>% 
  arrange(testing_program,group,update_date) %>% 
  filter(keep=="keep") %>% 
  filter(cat!="UF Total") 



my_data<-my_data %>% 
  select("update_date",
         "testing_program","group","cat","Npos",
         "Ntest","new.tests", "new.pos", "daily.pos") %>% 
  gather(metric,value,"Npos":"daily.pos")
my_data$metric<-as.factor(my_data$metric)

my_data<-my_data %>% 
  filter(metric=="daily.pos")


my_data$metric<-as.character(my_data$metric)

my_data$metric<-gsub("daily.pos", "% positive",my_data$metric)
my_data$metric<-as.factor(my_data$metric)


#
# gaps <- my_data %>%
#   arrange(testing_program,group,update_date) %>% 
#   group_by(testing_program,group) %>%
#   filter(is.nan(lead(daily.pos)) & row_number() != n() |
#            is.nan(lag(daily.pos)) & row_number() != 1)  %>%
#   mutate(gap.group = cumsum(row_number() %% 2)) 
# 


# The plot
UF.plot.daily <- ggplot(
  data = my_data,
  mapping = aes(
    x=update_date,
    y=value,
    color=cat)) +
  # scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d",expand = c(0, .9)) +
  facet_grid(rows = vars(cat),
             cols = vars(metric),
             scales = "fixed")+
  # scale_x_date(date_minor_breaks = "3 day")+
  geom_line(size=1) +
  geom_line(data = filter(my_data, is.na(value)==FALSE), linetype = "dashed") +
  # geom_line(data = filter(gaps, group == "fac.staff" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
  # geom_line(data = filter(gaps, group == "students" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
  # geom_line(data = filter(gaps, group == "students" & testing_program == "SHCC"), aes(group = gap.group), linetype = "dashed") +
  geom_point(size=1) +
  geom_hline((aes(yintercept=-Inf)), color="black") + 
  # scale_shape_manual(values = c(21, 21, 21)) +
  # scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_manual(values = c("blue4","coral3", "darkred"))+
  geom_text(aes(label = paste(round(value, 1),"%",sep="")), hjust = 0.6, vjust = -1, show.legend = FALSE, size=4) +
  # geom_text(aes(label = paste(round(value, 1),"%",sep="")), hjust = 0.6, vjust = -1, show.legend = FALSE) +
  labs(x = "Date", y = "Percent Positive (Daily)") +
  scale_y_continuous(limits = c(0, 80),breaks = seq(0,80, by=10),expand=c(0,0.1))+
  ggtitle(label = "UF - Positive tests per day (%)",
          subtitle = "Source: UF COVID19 Testing Dashboard")

UF.plot.daily <- UF.plot.daily + theme_classic() +
  theme(
    legend.title = element_blank(),
    panel.spacing.x =unit(1, "lines"), 
    panel.spacing.y=unit(2,"lines"),
    # legend.position = c(0.2, 0.9),
    legend.position = "none",
    strip.text.y = element_text(size = 14, angle=0),
    strip.text.x = element_blank(),
    strip.background.x = element_blank(),
    strip.background.y = element_rect(fill = NA, colour = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(colour = "black", size = 16),
    axis.title.y = element_text(colour = "black", size = 16),
    plot.margin =unit(c(1,1,1,1), "lines"),  
    legend.text = element_text(colour = "black", size = 10, vjust = 0.5),
    plot.title = element_text(size = 24,hjust = 0.5),
    plot.subtitle = element_text(size = 14,hjust = 0.5),
  )
UF.plot.daily
return(UF.plot.daily)
}