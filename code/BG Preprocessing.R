rm(list=ls())

library(dplyr)
library(lubridate)
library(stringr)

csv_folder_path <- "../data/BG EDA/"
csv_file_names <- list.files(csv_folder_path)

#Remove files with more than 10% incorrectly recorded blood glucose values from the experiment
remove_csv <- paste0(c("S018", "S091", "S105", "S109", "S122", "S131", "S133", "S138"), ".csv")

#Change in file count(140 -> 132)
csv_file_names <- setdiff(csv_file_names, remove_csv)

#Merge data
bg_df <- data.frame()
for(csv_name in csv_file_names){
  f <- read.csv(paste0(csv_folder_path, csv_name)) %>% 
    select(-insulin, -carbo) %>%
    filter(event=="EGV")
  
  f2 <- f %>% 
    mutate(glucose = ifelse(glucose=="낮음", 60, ifelse(glucose=="높음", 400, glucose))) %>% 
    mutate(glucose = as.numeric(glucose),
           time_stamp = ymd_hms(time_stamp))
  
  f3 <- f2 %>% 
    mutate(time_lag = as.duration(interval(lag(time_stamp), time_stamp))) %>% 
    mutate(time_sec = time_lag/as.duration(seconds(1)))
  
  time_sec_df <- f3 %>% filter(time_sec>600)
  
  if(nrow(time_sec_df)>0){
    time_group <- rep(NA, nrow(f3))
    for(i in 1:nrow(time_sec_df)){
      group <- ifelse(f3$time_stamp<time_sec_df$time_stamp[i], i, NA)
      time_group <- ifelse(is.na(time_group), group, time_group)
    }
    time_group <- ifelse(is.na(time_group), length(unique(time_group)), time_group) 
    mode_v <- which.max(table(time_group)) %>% as.vector()
    tf <- ifelse(time_group==mode_v, TRUE, FALSE)
    
    result <- f3[tf,] %>% 
      mutate(file_name = str_split(csv_name, ".csv")[[1]][1]) %>% 
      select(file_name, time_stamp, glucose, time_lag, time_sec)
  }else{
    result <- f3 %>% 
      mutate(file_name = str_split(csv_name, ".csv")[[1]][1]) %>% 
      select(file_name, time_stamp, glucose, time_lag, time_sec)
  }
  bg_df <- rbind(bg_df, result)
}

#Change in file count(132 -> 130)
bg_df2 <- bg_df %>% 
  group_by(file_name) %>% 
  filter(length(file_name)>100)

write.csv(bg_df2, "../data/BG dataset.csv", row.names=F)
