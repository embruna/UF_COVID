student_daily<-function(uf.data) {


my_data<-uf.data %>%
  filter(cat!="UF Total") %>% 
  filter(group=="students")
  
my_data<-my_data %>% 
  select("update_date",
         "testing_program","group","cat","Npos",
         "Ntest","new.tests", "new.pos", "daily.pos") %>% 
  gather(metric,value,"Npos":"daily.pos")
my_data$metric<-as.factor(my_data$metric)

my_data<-my_data %>% 
  filter(metric=="daily.pos" | metric=="new.pos"| metric=="new.tests")


my_data$metric<-as.character(my_data$metric)
my_data$metric<-gsub("new.tests", "Number\ntested",my_data$metric)
my_data$metric<-gsub("new.pos", "Number\npositive",my_data$metric)
my_data$metric<-gsub("daily.pos", "% positive",my_data$metric)
my_data$metric<-as.factor(my_data$metric)


my_data<-my_data %>% mutate(value=ifelse(value<0,NA,value))

##### PLOT % POS
Plot1 <- ggplot(
  data = filter(my_data,metric=="% positive"),
  mapping = aes(
    x = update_date,
    y = value,
    color=testing_program,
    shape=metric))+
  facet_grid(cols = vars(cat),
             rows=vars(metric),
             scales = "fixed")+
  geom_hline((aes(yintercept=-Inf)), color="black") + 
  geom_vline((aes(xintercept=-Inf)), color="black") + 
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d",expand = c(0, .9)) +
  geom_line() +
  geom_point(size=1) +
  geom_line(data = filter(my_data,metric=="% positive" & is.na(value)==FALSE), linetype = "dashed") +
  
  geom_text(aes(label = round(value, 1)), hjust = .9, vjust = -.7, size=5,show.legend = FALSE) +
  scale_shape_manual(values = c(15)) +
  scale_color_manual(values = c("#0021A5","#D7182A"))+
  labs(x = " ", y = "Percent") +
  scale_y_continuous(limits = c(0, 60),breaks = seq(0,60, by=10),expand=c(0,0.1))
  Plot1 <- Plot1 + theme_classic() +
               theme(
                 legend.title = element_blank(),
                 # legend.position = c(0.2, 0.9),
                 legend.position = "none",
                 axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                 axis.text.y = element_text(size = 14),
                 axis.title.x = element_text(colour = "black", size = 16),
                 axis.title.y = element_text(colour = "black", size = 16),
                 plot.title = element_text(size = 24),
                 # plot.margin =unit(c(1,1,5,1), "lines"),  
                 strip.text.x = element_text(size = 30,margin = margin(5,0,3,0, "lines")),
                 strip.text.y = element_text(size = 30, angle=0),
                 strip.background.x = element_rect(fill = NA, colour = NA),
                 strip.background.y = element_rect(fill = NA, colour = NA),
                 panel.spacing.x =unit(1, "lines"), 
                 panel.spacing.y=unit(2,"lines"),
                 legend.text = element_text(colour = "black", size = 10, vjust = 0.5)
               )
             

  
  ##### PLOT N tests pos
  Plot2 <- ggplot(
    data = filter(my_data,metric=="Number\npositive"),
    mapping = aes(
      x = update_date,
      y = value,
      color=testing_program,
      shape=metric))+
    facet_grid(cols = vars(cat),
               rows=vars(metric),
               scales = "fixed")+
    geom_hline((aes(yintercept=-Inf)), color="black") + 
    geom_vline((aes(xintercept=-Inf)), color="black") + 
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d",expand = c(0, .9)) +
    geom_line() +
    geom_point(size=1) +
    geom_line(data = filter(my_data,metric=="Number\npositive" & is.na(value)==FALSE), linetype = "dashed") +
    
    geom_text(aes(label = round(value, 1)), hjust = .9, vjust = -.7, size=5,show.legend = FALSE) +
    scale_shape_manual(values = c(15)) +
    scale_color_manual(values = c("#0021A5","#D7182A"))+
    labs(x = " ", y = "N") +
    scale_y_continuous(limits = c(0, 150),breaks = seq(0,150, by=15),expand=c(0,0.1))
  Plot2 <- Plot2 + theme_classic() +
    theme(
      legend.title = element_blank(),
      # legend.position = c(0.2, 0.9),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(colour = "black", size = 16),
      axis.title.y = element_text(colour = "black", size = 16),
      plot.title = element_text(size = 24),
      # plot.margin =unit(c(1,1,5,1), "lines"),  
      strip.text.x = element_text(size = 30,margin = margin(5,0,3,0, "lines")),
      strip.text.y = element_text(size = 30, angle=0),
      # strip.text.x = element_blank(),
      strip.background.x = element_blank(),
      strip.background.y = element_rect(fill = NA, colour = NA),
      panel.spacing.x =unit(1, "lines"), 
      panel.spacing.y=unit(2,"lines"),
      legend.text = element_text(colour = "black", size = 10, vjust = 0.5)
    )
  Plot2
  
      

  
  
  
  ##### PLOT N tests
  Plot3 <- ggplot(
    data = filter(my_data,metric=="Number\ntested"),
    mapping = aes(
      x = update_date,
      y = value,
      color=testing_program,
      shape=metric))+
    facet_grid(cols = vars(cat),
               rows=vars(metric),
               scales = "fixed")+
    geom_hline((aes(yintercept=-Inf)), color="black") + 
    geom_vline((aes(xintercept=-Inf)), color="black") + 
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d",expand = c(0, .9)) +
    geom_line() +
    geom_point(size=1) +
    geom_line(data = filter(my_data,(metric=="Number\ntested" & (is.na(value)==FALSE)|value<0)), linetype = "dashed") +
    geom_text( aes(label = round(value, 1)), hjust = .9, vjust = -.7, size=5,show.legend = FALSE) +
    scale_shape_manual(values = c(15)) +
    scale_color_manual(values = c("#0021A5","#D7182A"))+
    labs(x = " ", y = "N") +
    scale_y_continuous(limits = c(0, 600),breaks = seq(0,600, by=50),expand=c(0,0.1))
  Plot3 <- Plot3 + theme_classic() +
    theme(
      legend.title = element_blank(),
      # legend.position = c(0.2, 0.9),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(colour = "black", size = 16),
      axis.title.y = element_text(colour = "black", size = 16),
      plot.title = element_text(size = 24),
      # plot.margin =unit(c(1,1,5,1), "lines"),  
      # strip.text.x = element_text(size = 30,margin = margin(5,0,3,0, "lines")),
      strip.text.y = element_text(size = 30, angle=0),
      strip.text.x = element_blank(),
      strip.background.x = element_blank(),
      strip.background.y = element_rect(fill = NA, colour = NA),
      panel.spacing.x =unit(1, "lines"), 
      panel.spacing.y=unit(2,"lines"),
      legend.text = element_text(colour = "black", size = 10, vjust = 0.5)
    )
  Plot3
  student1<-Plot1
  student2<-grid.arrange(Plot2, Plot3, ncol=1)
  return(list(student1,student2))
}

  
#   
#   
# UF.plot.daily <- ggplot(
#   data = my_data,
#   mapping = aes(
#   x = update_date,
#   y = value,
#   color=testing_program,
#   shape=metric))+
#   facet_grid(cols = vars(label),
#   rows=vars(metric),
#   scales = "free")+
#   geom_hline((aes(yintercept=-Inf)), color="black") + 
#   geom_vline((aes(xintercept=-Inf)), color="black") + 
#   scale_x_date(date_breaks = "1 day", date_labels = "%b %d",expand = c(0, .9)) +
#   geom_line() +
#   geom_point(size=1) +
#   geom_line(data = filter(my_data, is.na(value)==FALSE), linetype = "dashed") +
#   geom_text(aes(label = round(value, 1)), hjust = .9, vjust = -.7, size=5,show.legend = FALSE) +
#   scale_shape_manual(values = c(15, 16, 17)) +
#   scale_color_manual(values = c("#0021A5","#D7182A"))+
#   # scale_shape_manual(values = c(15, 16, 17,18)) +
#                # scale_color_manual(values = c("gray65","gray65","gray65","darkred"))+
#                # # geom_text(aes(label = round(daily.pos, 1)), hjust = 0.6, vjust = -1, show.legend = FALSE) +
#                # geom_text(data = filter(my_data, (update_date==max(update_date)&label=="UF Total")), aes(label=label), 
#                #           nudge_x = -0.2, nudge_y=40, size=10,fontface="bold",show.legend = FALSE) +
#                # geom_text(data = filter(my_data, (update_date==max(update_date)&label!="UF Total")), aes(label=label), 
#                #           nudge_x = -0.1, nudge_y=40, size=6,show.legend = FALSE) +
#                # 
#                # labs(x = "Date", y = "Percent") +
#                # scale_y_continuous(limits = c(0, 60),breaks = seq(0,60, by=10),expand=c(0,0.1))+
#   labs(title = "UF - COVID-19 Data (Daily)")
#   UF.plot.daily <- UF.plot.daily + theme_classic() +
#   theme(
#   legend.title = element_blank(),
#   # legend.position = c(0.2, 0.9),
#   legend.position = "none",
#   axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
#   axis.text.y = element_text(size = 12),
#   axis.title.x = element_text(colour = "black", size = 16),
#   axis.title.y = element_text(colour = "black", size = 16),
#   plot.title = element_text(size = 24),
#   # plot.margin =unit(c(1,1,5,1), "lines"),  
#                  strip.text.x = element_text(size = 30,margin = margin(5,0,3,0, "lines")),
#                  strip.text.y = element_text(size = 30, angle=0),
#                  strip.background.x = element_rect(fill = NA, colour = NA),
#                  strip.background.y = element_rect(fill = NA, colour = NA),
#                  panel.spacing.x =unit(1, "lines"), 
#                  panel.spacing.y=unit(2,"lines"),
#                  legend.text = element_text(colour = "black", size = 10, vjust = 0.5)
#                )            
#              
#              
#              
#              
#              UF.plot.daily
#              return(UF.plot.daily)
# }






















#########################
# to do as single plots)
# 
# 
# my_data<-uf.data %>%
#   arrange(testing_program,group,update_date) %>% 
#   group_by(testing_program,group) %>% 
#   mutate(keep=ifelse(is.na(daily.pos)&lag(is.na(daily.pos)==TRUE),"del","keep")) %>% 
#   arrange(testing_program,group,update_date) %>% 
#   filter(keep=="keep")
#          
# gaps <- my_data %>%
#   arrange(testing_program,group,update_date) %>% 
#   group_by(testing_program,group) %>%
#   filter(is.nan(lead(daily.pos)) & row_number() != n() |
#            is.nan(lag(daily.pos)) & row_number() != 1)  %>%
#   mutate(gap.group = cumsum(row_number() %% 2)) 
# 
# 
# 
# 
# # The plot
# UF.plot.daily <- ggplot(
#   data = my_data,
#   mapping = aes(
#     x = update_date,
#     color=label,
#     shape=label))+
#   facet_grid(cols = vars(label), 
#              rows=vars(Region),
#   scale_x_date(date_breaks = "1 day", date_labels = "%b %d",expand = c(0, .9)) +
#   geom_line() +
#   geom_line(data = filter(my_data, is.na(daily.pos)==FALSE), linetype = "dashed") +
#   
#   geom_line(data = filter(gaps, group == "fac.staff" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
#   geom_line(data = filter(gaps, group == "students" & testing_program == "RTC"), aes(group = gap.group), linetype = "dashed") +
#   geom_line(data = filter(gaps, group == "students" & testing_program == "SHCC"), aes(group = gap.group), linetype = "dashed") +
#   geom_point(size=3) +
#   # geom_text(data = filter(my_data,label=="UF Total"), aes(label = round(Npos, 1)), hjust = .9, vjust = -.7, size=8,show.legend = FALSE) +
#   # geom_text(data = filter(my_data,label!="UF Total"), aes(label = round(Npos, 1)), hjust = .6, vjust = -1, size=6,show.legend = FALSE) +
#   # 
#   # geom_point(size=2) +
#   # scale_shape_manual(values = c(21, 21, 21)) +
#   # scale_color_brewer(palette = "Dark2") +
#   scale_shape_manual(values = c(15, 16, 17,18)) +
#   scale_color_manual(values = c("gray65","gray65","gray65","darkred"))+
#   # geom_text(aes(label = round(daily.pos, 1)), hjust = 0.6, vjust = -1, show.legend = FALSE) +
#   geom_text(data = filter(my_data, (update_date==max(update_date)&label=="UF Total")), aes(label=label), 
#             nudge_x = -0.2, nudge_y=40, size=10,fontface="bold",show.legend = FALSE) +
#   geom_text(data = filter(my_data, (update_date==max(update_date)&label!="UF Total")), aes(label=label), 
#             nudge_x = -0.1, nudge_y=40, size=6,show.legend = FALSE) +
#   
#   labs(x = "Date", y = "Percent") +
#   scale_y_continuous(limits = c(0, 60),breaks = seq(0,60, by=10),expand=c(0,0.1))+
#   labs(title = "UF - Percent of Tests Per Day Positive (Reported)")
# UF.plot.daily <- UF.plot.daily + theme_classic() +
#   theme(
#     legend.title = element_blank(),
#     # legend.position = c(0.2, 0.9),
#     legend.position = "none",
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
#     axis.text.y = element_text(size = 12),
#     axis.title.x = element_text(colour = "black", size = 16),
#     axis.title.y = element_text(colour = "black", size = 16),
#     plot.title = element_text(size = 24),
#     # plot.margin =unit(c(1,1,5,1), "lines"),  
#     legend.text = element_text(colour = "black", size = 10, vjust = 0.5)
#   )
# UF.plot.daily
# return(UF.plot.daily)
# }