require(shinydashboard)
require(shiny)
library(DT)
require(rpart)
require(rpart.plot)
library(ggplot2)
library(lubridate)
library(dplyr)
#source("global.R")





df <- read.csv("C:\\Users\\remn\\usr\\dev\\impact_calc\\exemplo.csv",encoding='latin1')
df_plot <- df %>%
          group_by(pk_dt, score_bins) %>%
              summarise(sum_vl_contr =sum(vl_contr),
                        sum_vl_perf  =sum(vl_perf),
                        sum_qtd_perf =sum(perf),
                        cnt_qtd_cnpj =n())
              
df_plot$perf_fis <- df_plot$sum_qtd_perf/df_plot$cnt_qtd_cnpj
df_plot$perf_fin <- df_plot$sum_vl_perf/df_plot$sum_vl_contr
#df_plot$pk_dt <- as.Date(df_plot$pk_dt)


get_DT <- function (data, refs, fl_train=1, x_varname){
      
      if(fl_train==1){
        data_ <- data %>%
          filter(pk_dt %in% as.numeric(refs)) %>%
            group_by(score_bins, !!as.name(x_varname)) %>%
              summarise(sum_vl_contr =sum(vl_contr),
                        sum_vl_risco =sum(vl_risco),
                        sum_vl_perf  =sum(vl_perf),
                        sum_qtd_perf =sum(perf),
                        cnt_qtd_cnpj =n())
        
        num_refs <- length(refs)
      }
      else{
        data_ <- data %>%
          filter(!pk_dt %in% as.numeric(refs)) %>%
            group_by(score_bins,  !!as.name(x_varname)) %>%
              summarise(sum_vl_contr =sum(vl_contr),
                        sum_vl_risco =sum(vl_risco),
                        sum_vl_perf  =sum(vl_perf),
                        sum_qtd_perf =sum(perf),
                        cnt_qtd_cnpj =n()) 
        
        num_refs <- length(unique(data$pk_dt)) - length(refs)
      }
  
  
      data_$perf_fis <- data_$sum_qtd_perf/data_$cnt_qtd_cnpj
      data_$perf_fin <- data_$sum_vl_perf/data_$sum_vl_contr
      data_$vl_risco <- data_$sum_vl_risco/(1000000 * num_refs) # average value in time
      data_$vl_contr <- data_$sum_vl_contr/(1000000 * num_refs) # average value in time
    
      # print(head(data_))
  
      return(data_)
}


generateDT <- function(data, x_varname, performance){
  data <- data %>% ungroup()
  y_cat <- sort(unique(data$score_bins))
  x_cat <- sort(unique(data[[x_varname]]))
  
  
  # dt$Bins[j] <- y_cat[j]
  
  # print(data)
  dt <- data.frame(Bins=y_cat)
  
  for(i in seq(1,length(x_cat))){
    dt[[as.character(x_cat[i])]] <- 0*seq(1,length(y_cat))
    vec <- numeric()
    for(j in seq(1,length(y_cat))){
      
      x <- data %>% 
              filter(!!as.name(x_varname) == x_cat[i] & score_bins == y_cat[j]) %>%
                select(!!as.name(performance))
      
      vec <- c(vec, as.numeric(x[1,1]))
      
      
      #dt[[x_cat[i]]][j] <- data[[performance]][which(df[[x_varname]] == x_cat[i] & df$score_bins == y_cat[j])]
    }
    dt[[as.character(x_cat[i])]] <- vec
  }
  
  # print(dt)
  return(dt)
}



