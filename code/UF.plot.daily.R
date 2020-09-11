UF.plot.daily<-function(uf.data) {

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
           is.nan(lag(daily.pos)) & row_number() != 1)  %>%
  mutate(gap.group = cumsum(row_number() %% 2)) 




# The plot
UF.plot.daily <- ggplot(
  data = my_data,
  mapping = aes(
    x = update_date,
    y = daily.pos,
    color = paste(testing_program, group, sep = " "))) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d",expand = c(0, .9)) +
  # scale_x_date(date_minor_breaks = "3 day")+
  geom_line() +
  geom_line(data = filter(my_data, is.na(daily.pos)==FALSE), linetype = "dashed") +
  # geom_line(data = filter(gaps, group == "fac.staff" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
  # geom_line(data = filter(gaps, group == "students" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
  # geom_line(data = filter(gaps, group == "students" & testing_program == "SHCC"), aes(group = gap.group), linetype = "dashed") +
  geom_point() +
  scale_shape_manual(values = c(21, 21, 21)) +
  scale_color_brewer(palette = "Dark2") +
  geom_text(aes(label = round(daily.pos, 1)), hjust = 0.6, vjust = -1, show.legend = FALSE) +
  labs(x = "Date", y = "Percent") +
  scale_y_continuous(limits = c(0, 60),breaks = seq(0,60, by=10),expand=c(0,0.1))+
  labs(title = "UF - Percent of Tests Per Day Positive (Reported)")
UF.plot.daily <- UF.plot.daily + theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2, 0.9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(colour = "black", size = 16),
    axis.title.y = element_text(colour = "black", size = 16),
    plot.title = element_text(size = 16),
    plot.margin =unit(c(1,1,1,1), "lines"),  
    legend.text = element_text(colour = "black", size = 12, vjust = 0.5)
  )

return(UF.plot.daily)
}