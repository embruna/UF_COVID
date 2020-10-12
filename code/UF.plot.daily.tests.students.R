UF.plot.daily.tests.students<-function(input) {
  # input<-bind
  # becauser there are some days with no data (weekends, etc)
  # there will be breaks in the line. This patches the breaks with a 
  # dashed line
  my_data_daily_tests<-input %>%
    arrange(testing_program,group,update_date) %>% 
    group_by(testing_program,group) %>% 
    mutate(keep=ifelse(is.na(new.tests)&lag(is.na(new.tests)==TRUE),"del","keep")) %>% 
    arrange(testing_program,group,update_date) %>% 
    filter(keep=="keep") %>% 
    mutate(new.tests=ifelse(new.tests<0,0,new.tests)) # There are values that are negative, must be corrections 
  # 
  # 
  # gaps_daily_tests <- my_data_daily_tests %>%
  #   arrange(testing_program,group,update_date) %>% 
  #   group_by(testing_program,group) %>%
  #   filter(is.nan(lead(new.tests)) & row_number() != n() |
  #            is.nan(lag(new.tests)) & row_number() != 1) %>%
  #   mutate(gap.group = cumsum(row_number() %% 2)) 
  # 
  
  
  my_data_daily_tests<-my_data_daily_tests %>% 
    select("update_date",
           "testing_program","group","cat","Npos",
           "Ntest","new.tests", "new.pos", "daily.pos") %>% 
    gather(metric,value,"Npos":"daily.pos")
  
  my_data_daily_tests$metric<-as.factor(my_data_daily_tests$metric)
  
  my_data_daily_tests<-my_data_daily_tests %>% 
    filter(metric=="new.pos"|metric=="new.tests") %>% 
    filter(cat!="UF Total") 
  
  
  
  my_data_daily_tests$metric<-as.character(my_data_daily_tests$metric)
  my_data_daily_tests$metric<-gsub("new.tests", "Number\nTested",my_data_daily_tests$metric)
  my_data_daily_tests$metric<-gsub("new.pos", "Number\nPositive",my_data_daily_tests$metric)
  my_data_daily_tests$metric<-as.factor(my_data_daily_tests$metric)
  
  
  my_data_daily_tests<-my_data_daily_tests %>% filter(cat!="Fac & Staff\n(RTC)")
  
  # my_data_daily_tests<-my_data_daily_tests %>% 
  #   spread(cat,value) %>%   
  #   replace_na(list(`Students\n(RTC)`=0,`Students\n(SHCC)`=0)) %>% 
  #   mutate('Students\nTotal'=`Students\n(RTC)`+`Students\n(SHCC)`) %>% 
  #   gather(cat,value,`Students\n(RTC)`:`Students\nTotal`)
  # The plot
  UF.plot.daily.tests <- ggplot(
    data = my_data_daily_tests,
    mapping = aes(
        x=update_date,
        y=value,
        color=cat)) +
      # scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      scale_x_date(date_breaks = "2 day", date_labels = "%b %d",expand = c(0, .9)) +
      facet_grid(rows = vars(metric),
                 # cols = vars(cat),
                 scales = "fixed")+
    geom_line() +
    geom_hline((aes(yintercept=-Inf)), color="black") + 
    geom_line(data = filter(my_data_daily_tests, is.na(value)==FALSE), linetype = "dashed") +
    # geom_line(data = filter(gaps_daily_tests, group == "fac.staff" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
    # geom_line(data = filter(gaps_daily_tests, group == "students" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
    # geom_line(data = filter(gaps_daily_tests, group == "students" & testing_program == "SHCC"), aes(group = gap.group), linetype = "dashed") +
    geom_point(size=1) +
    # scale_shape_manual(values = c(21, 21, 21)) +
    # scale_color_brewer(palette = "Dark2") +
    scale_shape_manual(values = c(15, 16, 17)) +
    scale_color_manual(values = c("blue4","coral3", "darkred"))+
    geom_text(aes(label = round(value, 1)), hjust = 0.6, vjust = -1, show.legend = FALSE, size=2) +
    scale_y_continuous(limits = c(0, max(my_data_daily_tests$value)+100),breaks = seq(0,max(my_data_daily_tests$value)+100, by=100),expand=c(0,0.1))+
    labs(x = "Date", y = "Count") +
    ggtitle(label = "UF - Tests per day (Positive & Total)",
            subtitle = "Source: UF COVID19 Testing Dashboard")
  UF.plot.daily.tests <- UF.plot.daily.tests + theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      panel.spacing.x =unit(1, "lines"), 
      panel.spacing.y=unit(2,"lines"),
      strip.text.y = element_text(size = 14, angle=0),
      strip.text.x = element_text(size = 14, angle=0),
      strip.background.x = element_blank(),
      strip.background.y = element_rect(fill = NA, colour = NA),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(colour = "black", size = 16),
      axis.title.y = element_text(colour = "black", size = 16),
      plot.margin =unit(c(1,1,1,1), "lines"),  
      legend.text = element_text(colour = "black", size = 10, vjust = 0.5),
      plot.title = element_text(size = 24,hjust = 0.5),
      plot.subtitle = element_text(size = 14,hjust = 0.5),
    )
  
UF.plot.daily.tests
  return(UF.plot.daily.tests)
}