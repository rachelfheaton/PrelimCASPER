setwd("~/AttentionModel Add Relations/DissertationCASPER/data/QO_EMC_30_ATC_2_0.9_0.15_0.001_4rfs")
source('~/AttentionModel Add Relations/DissertationCASPER/CASPER_O_Q_Asymmetry_helper_functions.R')
library(tidyverse)
 
library(viridis)
# all_files = list.files(pattern=".csv")
# all_data = data.frame()
# for (i in all_files) {
#   participant_data = read.csv(i)
#   rbind(participant_data, all_data)
# }

read_files <- function(){
  all_files <- list.files(pattern=".csv")
  for (file in all_files) {
    print(file)
    print(ncol(read.csv(file)))
  }
  do.call(rbind, lapply(all_files, read.csv, header=TRUE))
}


all_data <- read_files()

accuracy_df <- get_accuracy_df(all_data)

clean_df <- get_clean_df(all_data, accuracy_df)
#clean_df$block <- as.factor(clean_df$trials.thisRepN)

individual_df <- get_individual_df(clean_df, c("total_setsize", "dcolor"))

plot_df <- get_plot_df(individual_df, "dcolor")
#dotplot_df <- get_dotplot_df(individual_df, "dcolor")

plot_df$plot_color <- as.factor(plot_df$plot_color)

plot_log_curves(plot_df, 'plot_color',
                c("Q among O", "O among Q", "Target Only"))#, "Feature Only", "Target Only"))

#ggplot(clean_df, aes(x=total_setsize, y=resp.rt, color=dcolor)) +
#  geom_jitter(data=clean_df, aes(color=dcolor), size=1)
  
#ggplot(individual_df, aes(x=total_setsize, y=meanRT, color=dcolor)) +
 
#  geom_point(data=individual_df, aes(color=dcolor), size=4) +
#  geom_errorbar(aes(ymin=meanRT-SEM, ymax=meanRT+SEM, color=dcolor), width=1.2, size=1.2) +
  # geom_jitter(data=dotplot_df, aes(color=plot_color), size=4) +
  #scale_color_manual(values=c('#d4240d', '#f06a11', '#2abd73', '#2399d9'),
  #                   labels = c('Plot'))+
  #scale_color_viridis(discrete = TRUE, option = "D", labels=line_names) +
  #stat_smooth(method="lm", formula=y~log(x), se=FALSE, linetype=1, size=1.8) #+
  #theme(axis.text.x = element_text(size=32),
  #      axis.text.y = element_text(size=32),
  #      axis.title.x = element_text(size=32),
  #      axis.title.y = element_text(size=32),
  #      legend.title = element_text(size=28),
  #      legend.text = element_text(size=28)) +
  #ylab("RT(ms)\n") +
  #xlab("Setsize") #+ 
  #scale_y_continuous( limits=c(500,2500)) + scale_x_continuous(limits=c(0,20)) +
  #scale_y_continuous( limits=c(0,25)) + scale_x_continuous(breaks=c(2,4,8,16)) +
  # scale_y_continuous(limits = c(600, 1000),
  #                    breaks=seq(600, 1000, 100),
  #                    labels = every_nth(seq(600, 1000, 100), 1, inverse=TRUE)) +  
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "black"))


log_df <- individual_df %>% 
    mutate(setsize_log = log(total_setsize)) %>% 
    group_by(dcolor) %>%
    do(log_fit = lm(meanRT ~ setsize_log, data=.)) 


nontargetonly_df <- plot_df %>%
  filter(plot_color != 99)

human_data <- read.csv("~/AttentionModel Add Relations/DissertationCASPER/TriesmanSoutherEstimates.csv", header=TRUE)
human_df <- human_data %>%
  filter(plot_color != 99) 

ordered_human_data <- human_data[order(human_data$total_setsize),] 
#ordered_human_data <- ordered_human_data %>% filter(plot_color!=99)

ordered_plot_df <- plot_df[order(plot_df$total_setsize),]
#ordered_plot_df <- ordered_plot_df %>% filter(plot_color!=99)

max_human_setsize = max(ordered_human_data$total_setsize)
max_model_setsize = max(ordered_plot_df$total_setsize)
min_human_setsize = min(ordered_human_data$total_setsize)
min_model_setsize = min(ordered_plot_df$total_setsize)



human_RTs <- ordered_human_data %>%
  group_by(dcolor) %>%
  mutate(RT_range = max(RT)-min(RT)) %>%
  filter(total_setsize == max_human_setsize)

model_RTs <- ordered_plot_df %>%
  group_by(dcolor) %>%
  mutate(RT_range = max(RT)-min(RT))%>%
  filter(total_setsize == max_model_setsize)

ms_per_iteration <- human_RTs$RT_range/model_RTs$RT_range
print(ms_per_iteration)


cond1_human_df <- filter(ordered_human_data, dcolor==1)
cond2_human_df <- filter(ordered_human_data, dcolor==2)
cond3_human_df <- filter(ordered_human_data, dcolor==3)

cond1_model_df <- filter(ordered_plot_df, dcolor==1)
cond2_model_df <- filter(ordered_plot_df, dcolor==2)
cond3_model_df <- filter(ordered_plot_df, dcolor==3)

rsq <- function(x,y) summary(lm(y~x))$r.squared

cond1_rsq <- rsq(cond1_human_df$RT, cond1_model_df$RT)
cond2_rsq <- rsq(cond2_human_df$RT, cond2_model_df$RT)  
cond3_rsq <- rsq(cond3_human_df$RT, cond3_model_df$RT)

all_rsq <- rsq(ordered_human_data$RT, ordered_plot_df$RT)

plot(ordered_human_data$RT, ordered_plot_df$RT)











#dotplot_df$plot_color <- as.factor(dotplot_df$plot_color)

#ggplot(plot_df, aes(x=total_setsize, y=RT, color=dcolor)) +
  
#  geom_point(data=plot_df, aes(color=dcolor), size=2) +
#  geom_errorbar(aes(ymin=RT-SEM, ymax=RT+SEM, color=dcolor), width=1.2, size=1.2) +
  # geom_jitter(data=dotplot_df, aes(color=plot_color), size=4) +
#  scale_color_manual(values=c('#d4240d', '#f06a11', '#2abd73', '#2399d9'),
#                     labels = c("Intercept", "Other"))+
  #scale_color_viridis(discrete = TRUE, option = "D", labels=line_names) +
#  stat_smooth(method="lm", formula=y~log(x), se=FALSE, linetype=1, size=1.8) #+
#theme(axis.text.x = element_text(size=32),
#      axis.text.y = element_text(size=32),
#      axis.title.x = element_text(size=32),
#      axis.title.y = element_text(size=32),
#      legend.title = element_text(size=28),
#      legend.text = element_text(size=28)) +
#ylab("RT(ms)\n") +
#xlab("Setsize") #+ 
#scale_y_continuous( limits=c(500,2500)) + scale_x_continuous(limits=c(0,20)) +
#scale_y_continuous( limits=c(0,25)) + scale_x_continuous(breaks=c(2,4,8,16)) +
# scale_y_continuous(limits = c(600, 1000),
#                    breaks=seq(600, 1000, 100),
#                    labels = every_nth(seq(600, 1000, 100), 1, inverse=TRUE)) +  
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),


#plot_scatter_curves(dotplot_df, 'plot_color',
#                c( "Green O over Red X", "Green O over Orange X", "Orange X over Green O", "Target Only"))



log_df <- plot_df %>% 
  mutate(setsize_log = log(total_setsize)) %>% 
  group_by(dcolor) %>%
  do(log_fit = lm(RT ~ setsize_log, data=.)) 


lapply(log_df$log_fit, summary.lm)
log_summary_df <- lapply(log_df$log_fit, summary.lm)
log_out <- lapply(log_summary_df , function(x) x$r.squared )



linear_df <- plot_df %>%
  group_by(dcolor) %>%
  do(linear_fit = lm(RT ~ total_setsize, data=.))


lapply(linear_df$linear_fit, summary.lm)
linear_summary_df <- lapply(linear_df$linear_fit, summary.lm)
linear_out <- lapply(linear_summary_df , function(x) x$r.squared )
# plot(log_out,linear_out, xlim=c(0,1), ylim=c(0,1))
# abline(0,1)


typeof(individual_df$total_setsize)
print(individual_df$total_setsize)

individual_df$total_setsize <- as.numeric(individual_df$total_setsize)

require(broom)
out_df <- data.frame()

individual_df <- subset(individual_df, dcolor != "0")
out_df <- individual_df %>%
  mutate(setsize_log = log(total_setsize)) %>%
  group_by(participant,dcolor) %>% 
  do(log_fit = tidy(lm(meanRT ~ setsize_log, data=.))) %>%
  unnest(log_fit)
out_df$dcolor <- factor(out_df$dcolor)
log_slopes <- get_slopes(out_df)

library(ez)

ezANOVA(log_slopes,
        dv=estimate,
        wid=participant,
        within=dcolor)

t.test((log_slopes %>% filter(dcolor=="1"))$estimate,
       (log_slopes %>% filter(dcolor == "2"))$estimate,
       paired=TRUE)

t.test((log_slopes %>% filter(dcolor=="2"))$estimate,
       (log_slopes %>% filter(dcolor == "3"))$estimate,
       paired=TRUE)

t.test((log_slopes %>% filter(dcolor=="1"))$estimate,
       (log_slopes %>% filter(dcolor == "3"))$estimate,
       paired=TRUE)



# for(i in seq_along(levels(individual_df$block))){
#   if(i != length(levels(individual_df$block))){
#     if (i==1 | i==23) {
#       print(i)
#       block_individual_df <- individual_df %>% filter(block %in% c(levels(block)[i], levels(block)[i+1]))
#     
#       block_plot_df <- get_plot_df(block_individual_df, "dcolor")
#       block_plot_df$plot_color <- as.factor(block_plot_df$plot_color)
#       plot <- plot_log_curves(block_plot_df, 'plot_color',  c("Relation Only", "Relation + Feature", "Feature Only", "Target Only"))
#       print(plot)
#     }
#   }
#   
# }
