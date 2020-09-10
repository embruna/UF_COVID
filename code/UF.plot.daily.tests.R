UF.plot.daily.tests<-function(uf.data) {
  
  # becauser there are some days with no data (weekends, etc)
  # there will be breaks in the line. This patches the breaks with a 
  # dashed line
  my_data_daily_tests<-uf.data %>%
    arrange(testing_program,group,update_date) %>% 
    group_by(testing_program,group) %>% 
    mutate(keep=ifelse(is.na(new.tests)&lag(is.na(new.tests)==TRUE),"del","keep")) %>% 
    arrange(testing_program,group,update_date) %>% 
    filter(keep=="keep")
  
  gaps_daily_tests <- my_data_daily_tests %>%
    arrange(testing_program,group,update_date) %>% 
    group_by(testing_program,group) %>%
    filter(is.nan(lead(new.tests)) & row_number() != n() |
             is.nan(lag(new.tests)) & row_number() != 1) %>%
    mutate(gap.group = cumsum(row_number() %% 2)) 
  
  
  
  # The plot
  UF.plot.daily.tests <- ggplot(
    data = my_data_daily_tests,
    mapping = aes(
      x = update_date,
      y = new.tests,
      color = paste(testing_program, group, sep = " "))) +
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
    # scale_x_date(date_minor_breaks = "3 day")+
    geom_line() +
    geom_line(data = filter(gaps_daily_tests, group == "fac.staff" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
    geom_line(data = filter(gaps_daily_tests, group == "students" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
    geom_line(data = filter(gaps_daily_tests, group == "students" & testing_program == "SHCC"), aes(group = gap.group), linetype = "dashed") +
    geom_point() +
    scale_shape_manual(values = c(21, 21, 21)) +
    scale_color_brewer(palette = "Dark2") +
    geom_text(aes(label = round(new.tests, 1)), hjust = 0, vjust = -1, show.legend = FALSE) +
    scale_y_continuous(limits = c(0, 800),breaks = seq(0,700, by=100),expand=c(0,0.1))+
    labs(x = "Date", y = "Number of Tests") +
    labs(title = "UF - Number of Tests Per Day (reported)")
  UF.plot.daily.tests <- UF.plot.daily.tests + theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = c(0.8, 0.8),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(colour = "black", size = 16),
      axis.title.y = element_text(colour = "black", size = 16),
      plot.title = element_text(size = 16),
      plot.margin =unit(c(1,1,1,1), "lines"),
      legend.text = element_text(colour = "black", size = 12, vjust = 0.5)
    )
  UF.plot.daily.tests
  return(UF.plot.daily.tests)
}