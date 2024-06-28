library(tidyverse)
library(broom)
library(viridis)

read_files <- function(){
  all_files <- list.files(pattern=".csv")
  print(all_files)
  do.call(rbind, lapply(all_files, read.csv, header=TRUE))
}


get_accuracy_df <- function(df, remove_timeouts=TRUE){
  
  ## df: df containing all data
  
  if(remove_timeouts){
    rt_max <- 999999999
  } else{
    rt_max <- 9999
  }
  
  out_df <- df %>% 
    filter(trial_type == "expt") %>% 
    filter(resp.rt < rt_max) %>% 
    filter(resp.rt > 0.300) %>%
    group_by(participant) %>% 
    summarise(accuracy = round(mean(resp.corr) * 100, 2))
}


get_clean_df <- function(df, accuracy_df, min_accuracy=90){
  
  ## df: df containing all data
  ## accuracy_df: df of individual subject accuracy
  
  bad_subs <- (accuracy_df %>%
                 filter(accuracy < min_accuracy))$participant
  print(bad_subs)
  
  good_subs_df <- df %>% 
    filter(!is.element(participant, bad_subs))
  
  correct_trials_df <- good_subs_df %>% 
    filter(trial_type == "expt",
           resp.corr == 1, resp.rt > 0.300)
  
  print(correct_trials_df)
  
  clean_df <- correct_trials_df %>%
    group_by(participant) %>% 
    mutate(meanRT = mean(resp.rt),
           sdRT = sd(resp.rt)) %>% 
    # mutate(outlier = if_else(resp.rt < meanRT - (2.5*sdRT) | resp.rt > meanRT + (2.5*sdRT), 1, 0)) %>% 
    # filter(outlier == 0) 
  
  print(paste("Low accuracy participants: " , length(bad_subs), " participants removed", sep=""))
  
  print(paste("Incorrect response: ", 
              as.character(signif((nrow(good_subs_df) - nrow(correct_trials_df)) / nrow(good_subs_df) * 100), 3), "% trials removed", sep=""))
  
  print(paste("Outliers (2.5 SD): ", 
              as.character(signif((nrow(correct_trials_df) - nrow(clean_df)) / nrow(correct_trials_df) * 100), 3), "% trials removed", sep=""))
  
  print(paste("Total participants: ",
              length(unique(clean_df$participant))))
  return(clean_df)    
  
  
}



get_individual_df <- function(df, iv_vector){
  
  ## df: cleaned df (typically from get_cleaned_df)
  ## iv_vector: a vector of strings of IVs to group by
  
  iv_vars <- lapply(iv_vector, as.symbol)
  
  out_df <- df %>% 
    group_by(participant, .dots=iv_vars) %>% 
    summarise(meanRT = mean(resp.rt),
              sdRT = sd(resp.rt))
  
  out_df$participant <- as.factor(out_df$participant)
  
  return(out_df)
}


get_plot_df <- function(df, iv_str){
  
  ## df: individual-level df (typically from get_individual_df)
  ## iv_str: string of IV (e.g. "distractor_type")
  
  iv_var <- sym(iv_str)
  
  out_df <- df %>%
    group_by(total_setsize, !!iv_var) %>%
    summarise(RT = mean(meanRT),
              SEM = sd(meanRT)/sqrt(length(unique(df$participant))))

  target_only <- out_df %>%
    filter(total_setsize == 1)
  
  target_only <- do.call("rbind", replicate(length(unique(out_df[[iv_str]])), target_only, simplify = FALSE))
  target_only[[iv_str]] <- unique(out_df[[iv_str]])

  out_df <- rbind(out_df, target_only) %>%
    distinct(total_setsize, !!iv_var, .keep_all = TRUE) %>%
    filter(dcolor > 0) %>%
    mutate(plot_color = ifelse(total_setsize==1, 99, !!iv_var))

  out_df$plot_color <- as.factor(out_df$plot_color)
  out_df[[iv_str]] = as.factor(out_df[[iv_str]])
  
  return(out_df)
}

# get_plot_df <- function(df, iv_str){
#   
#   ## df: individual-level df (typically from get_individual_df)
#   ## iv_str: string of IV (e.g. "distractor_type")
#   
#   iv_var <- sym(iv_str)
#   
#   out_df <- df %>% 
#     group_by(total_setsize, !!iv_var) %>% 
#     summarise(RT = mean(meanRT),
#               SEM = sd(meanRT)/sqrt(length(unique(df$participant)))) 
#   # out_df <- df %>% 
#   #   group_by(total_setsize, !!iv_var) %>% 
#   #   summarise(RT = mean(meanRT),
#   #             SEM = sd(meanRT)/sqrt(length(unique(df$participant)))) 
#   # 
#   target_only <- out_df %>% 
#     filter(total_setsize == 1)
#   
#   target_only <- do.call("rbind", replicate(length(unique(out_df[[iv_str]])), target_only, simplify = FALSE))  
#   target_only[[iv_str]] <- unique(out_df[[iv_str]])
#   
#   out_df <- rbind(out_df, target_only) %>% 
#     distinct(total_setsize, !!iv_var, .keep_all = TRUE) %>% 
#     filter(dcolor > 0) %>% 
#     mutate(plot_color = ifelse(total_setsize==1, 99, !!iv_var))
#   
#   out_df$plot_color <- as.factor(out_df$plot_color)
#   out_df[[iv_str]] = as.factor(out_df[[iv_str]])
#   
#   return(out_df)
# }

get_dotplot_df <- function(df, iv_str){
    
    ## df: individual-level df (typically from get_individual_df)
    ## iv_str: string of IV (e.g. "distractor_type")
    
    iv_var <- sym(iv_str)
    
    out_df <- df %>% group_by(total_setsize, !!iv_var)
    print(head(out_df, 20))
    # target_only <- out_df %>% 
    # filter(total_setsize == 1)
    # 
    # print(target_only)
    # # 
    # target_only <- do.call("rbind", replicate(length(unique(out_df[[iv_str]])), target_only, simplify = FALSE))  
    # print(target_only)
    # print(out_df[[iv_str]])
    # print(head(out_df, 20))
    # #target_only[[iv_str]] <- unique(out_df[[iv_str]])
    # # 
    out_df <- out_df %>% 
    mutate(plot_color = ifelse(total_setsize==1, 99, !!iv_var))
    # 
    #out_df$participant <- as.factor(out_df$participant)
    out_df$plot_color <- as.factor(out_df$plot_color)
    #out_df$total_setsize <- as.factor(out_df$total_setsize)
    out_df[[iv_str]] = as.factor(out_df[[iv_str]])
    
    return(out_df)
  }
  

plot_log_curves <- function(df, iv_str, line_names){
  
  ## df: summarised df for plotting (typically from get_plot_df)
  ## iv_str: string of IV (e.g. "distractor_type")
  ## line_names: vector of labels for sepearte lines
  
  iv_var = sym(iv_str)
  
  ggplot(df, aes(x=total_setsize, y=RT, color=!!iv_var)) +
    geom_point(data=df, aes(color=plot_color), size=2) +
    geom_line(data=df, aes(color=plot_color), size=1.8) +
    geom_errorbar(aes(ymin=RT-SEM, ymax=RT+SEM, color=plot_color), width=1.2, size=1.2) +
   # geom_jitter(data=dotplot_df, aes(color=plot_color), size=4) +
    scale_color_manual(values=c('#2399d9','#f6be00', '#f06a11', '#d4240d'),
                        labels = line_names)+
    #scale_color_viridis(discrete = TRUE, option = "D", labels=line_names) +
    #stat_smooth(method="lm", formula=y~log(x), se=FALSE, linetype=1, size=1.8) +
    theme(axis.text.x = element_text(size=32),
          axis.text.y = element_text(size=32),
          axis.title.x = element_text(size=32),
          axis.title.y = element_text(size=32),
          legend.title = element_text(size=28),
          legend.text = element_text(size=28)) +
    ylab("RT(iterations)\n") +
    xlab("Setsize") + 
    #scale_y_continuous( limits=c(500,2500)) + scale_x_continuous(limits=c(0,20)) +
    scale_y_continuous( limits=c(0,200)) + scale_x_continuous(breaks=c(1,5,10,20,32)) +
    # scale_y_continuous( limits=c(0,25)) + scale_x_continuous(breaks=c(1,3,7,16,32)) +
    # scale_y_continuous(limits = c(600, 1000),
    #                    breaks=seq(600, 1000, 100),
    #                    labels = every_nth(seq(600, 1000, 100), 1, inverse=TRUE)) +  
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"))

}



plot_scatter_curves <- function(df, iv_str, line_names){
  
  ## df: summarised df for plotting (typically from get_plot_df)
  ## iv_str: string of IV (e.g. "distractor_type")
  ## line_names: vector of labels for sepearte lines
  
  iv_var = sym(iv_str)
  
  ggplot(df, aes(x=total_setsize, y=meanRT, color=!!iv_var)) +
    geom_jitter(data=df, aes(color=plot_color), size=4) +
    #geom_errorbar(aes(ymin=RT-SEM, ymax=RT+SEM, color=plot_color), width=1.2, size=1.2) + 
    scale_color_manual(values=c('#d4240d', '#f06a11', '#2399d9', '#2abd73'),
                       labels = line_names)+
    #scale_color_viridis(discrete = TRUE, option = "D", labels=line_names) +
    #stat_smooth(method="lm", formula=y~log(x), se=FALSE, linetype=1, size=1.8) +
    theme(axis.text.x = element_text(size=32),
          axis.text.y = element_text(size=32),
          axis.title.x = element_text(size=32),
          axis.title.y = element_text(size=32),
          legend.title = element_text(size=28),
          legend.text = element_text(size=28)) +
    ylab("RT(ms)\n") +
    xlab("Setsize") + 
    # scale_y_continuous(limits = c(600, 1000),
    #                    breaks=seq(600, 1000, 100),
    #                    labels = every_nth(seq(600, 1000, 100), 1, inverse=TRUE)) +  
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
}





##########################
###### WIP ################
#############################


run_regression <- function(individual_df){
  
  individual_df$total_setsize <- as.numeric(individual_df$total_setsize)
  out_df <- individual_df %>%
    mutate(setsize_log = log(total_setsize)) %>%
    group_by(participant,dcolor) %>% 
    do(log_fit = lm(meanRT ~ setsize_log, data=.)) %>%
    tidy(log_fit)
  
  out_df$participant <- as.factor(out_df$participant)
  out_df$dcolor <- as.factor(out_df$dcolor)
  
  out_df
  
} 

get_slopes <- function(regression_df){
  
  regression_df %>% filter(term == "setsize_log")
}



get_intercepts <- function(df){
  
  df %>%  filter(term == "(Intercept)")
}


get_estimate_descriptives <- function(df, iv_name){
  
  iv_var <- sym(iv_name)
  
  df %>%    
    group_by(!!iv_var) %>% 
    summarise(m = mean(estimate), sd = sd(estimate))
} 