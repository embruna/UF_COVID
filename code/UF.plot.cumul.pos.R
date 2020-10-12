UF.plot.cumul.pos<-function(input) {
  
  # becauser there are some days with no data (weekends, etc)
  # there will be breaks in the line. This patches the breaks with a 
  # dashed line
  # 
  # my_data<-uf.data %>%
  #   arrange(testing_program,group,update_date) %>% 
  #   group_by(testing_program,group) %>% 
  #   mutate(keep=ifelse(is.na(Npos)&lag(is.na(Npos)==TRUE),"del","keep")) %>% 
  #   arrange(testing_program,group,update_date) %>% 
  #   filter(keep=="keep") %>% 
  #   mutate(keep=ifelse(is.na(Npos)&lag(is.na(Npos)==TRUE),"del","keep")) %>% 
  #   arrange(testing_program,group,update_date) %>% 
  #   filter(keep=="keep") 
  
  #   scale_color_brewer(palette = "Blues") +
  #   scale_fill_manual(values = c("#6C9AC3", "#A8DCD9", "#E28F41"))+
  
  # gaps <- my_data %>%
  #   arrange(testing_program,group,update_date) %>% 
  #   group_by(testing_program,group) %>%
  #   filter(is.nan(lead(Npos)) & row_number() != n() |
  #            is.nan(lag(Npos)) & row_number() != 1) %>%
  #   mutate(gap.group = cumsum(row_number() %% 2)) 
  # 
  
  my_data<-input
  # The plot
  PlotCumm <- ggplot(
    data = my_data,
    mapping = aes(
      x = update_date,
      y = Npos,
      color=cat,
      shape=cat))+
      # color = paste(testing_program, group, sep = " "),
      # shape = paste(testing_program, group, sep = " "))) +
      # scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d",expand = c(.02, .09)) +
    geom_line(size=2) +
    # geom_line(data = filter(my_data, testing_program=="RTC" & group=="students")) +
    geom_line(data = filter(my_data, is.na(Npos)==FALSE), linetype = "dashed") +
    scale_shape_manual(values = c(15, 16, 17,18)) +
    scale_color_manual(values = c("gray65","gray65","gray65","darkred"))+
    # scale_color_manual(values = c("blue4","coral3", "darkred"))+
    # geom_line(data = filter(gaps, group == "fac.staff" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
    # geom_line(data = filter(gaps, group == "students" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
    # geom_line(data = filter(gaps, group == "students" & testing_program == "SHCC"), aes(group = gap.group), linetype = "dashed") +
    geom_point(size=1) +
    geom_text(data = filter(my_data,cat=="UF Total"&update_date==max(update_date)), 
              aes(label = round(Npos, 1)), nudge_y=45,nudge_x = -0.99, size=5,show.legend = FALSE) +
    # geom_text(data = filter(my_data,cat!="UF Total"&update_date==max(update_date)),
    #           aes(label = round(Npos, 1)), nudge_y = 30,nudge_x = -0.0, size=3,show.legend = FALSE) +
    
    geom_text(data = filter(my_data,testing_program=="RTC" &  group=="students" & update_date==max(update_date)),
              aes(label = round(Npos, 1)), nudge_y = 40,nudge_x = -0.0, size=3,show.legend = FALSE) +
    
    geom_text(data = filter(my_data,testing_program=="RTC" &  group=="fac.staff" & update_date==max(update_date)),
              aes(label = round(Npos, 1)), nudge_y = 40,nudge_x = -0, size=3,show.legend = FALSE) +
    
    geom_text(data = filter(my_data,testing_program=="SHCC" &  group=="students" & update_date==max(update_date)),
              aes(label = round(Npos, 1)), nudge_y = -50,nudge_x = -0.0, size=3,show.legend = FALSE) +
    
    # geom_text(aes(label = round(Npos, 1)), hjust = .6, vjust = -1, size=8,show.legend = FALSE) +
    # geom_text(aes(label = group), hjust = 1, vjust = -1, size=5) +
    # geom_text(data = filter(my_data, update_date==max(update_date)), aes(label=label), 
    #            nudge_x = -0.4, nudge_y=35, size=5, fontface="bold",show.legend = FALSE) +
    geom_text(data = filter(my_data, ((update_date==max(update_date) & cat=="UF Total"))), aes(label=cat),
              nudge_x = -0.99, nudge_y=130, size=5,fontface="bold",show.legend = FALSE) +
    
    geom_text(data = filter(my_data,testing_program=="RTC" &  group=="students" & update_date==max(update_date)),
              aes(label = cat), nudge_y = 130,nudge_x = -0.6, size=3,show.legend = FALSE) +
    
    geom_text(data = filter(my_data,testing_program=="RTC" &  group=="fac.staff" & update_date==max(update_date)),
              aes(label = cat), nudge_y = 125,nudge_x = -0.8, size=3,show.legend = FALSE) +
    
    geom_text(data = filter(my_data,testing_program=="SHCC" &  group=="students" & update_date==max(update_date)),
              aes(label = cat), nudge_y = -130,nudge_x = -0.6, size=3,show.legend = FALSE) +
    
    
    
    # geom_text(data = filter(my_data, ((update_date==max(update_date) & cat!="UF Total"))), aes(label=cat),
    #           nudge_x = -0.3, nudge_y=70, size=3,show.legend = FALSE) +
    labs(x = "Date", y = "Cumulative Count") +
    scale_y_continuous(limits = c(0, 2000),breaks = seq(0,2000, by=150),expand=c(0,0.2))+
    ggtitle(label = "UF - Positive Tests since 6 May 2020",
            subtitle = "Source: UF COVID19 Testing Dashboard")
  
  PlotCumm <- PlotCumm + theme_classic() +
    theme(
      legend.title = element_blank(),
      # legend.position = c(0.2, 0.9),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(colour = "black", size = 16),
      axis.title.y = element_text(colour = "black", size = 16),
      plot.title = element_text(size = 24,hjust = 0.5),
      plot.subtitle = element_text(size = 14,hjust = 0.5),
  
      # plot.margin =unit(c(1,1,5,1), "lines"),  
      legend.text = element_text(colour = "black", size = 10, vjust = 0.5)
    )
  PlotCumm
  
  
  return(PlotCumm)
}