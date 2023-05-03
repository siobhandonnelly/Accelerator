#Installing the packages used to load in and clean the dataset
# install.packages("tidyverse", dependencies = TRUE, type = "win.binary")
# install.packages("readxl", dependencies = TRUE, type = "win.binary")
# install.packages("janitor", dependencies = TRUE, type = "win.binary")

#Loading in the Tidyverse
library(tidyverse)
library(usethis)
library(ggplot2)
library(dplyr)
?use_github

# use_github(protocol = 'https', auth_token = Sys.getenv("GITHUB_PAT"))

#Loading in the dataset
nature_of_work <- read_csv("UKHSA dataset.csv")

#Trying to load in data with nulls de\lt with
nature_of_work <- read_csv(("UKHSA dataset.csv"), 
                           na = c("U", "Z0 - Missing data" ,"NA" ,"Not applicable", "-1","*",".", "", "'NULL'"))
#Taking a look at the dataset 
View(nature_of_work)
#Starting to amend variable types. Changing the f_acyear variable to a factor

# typeof("f_acyear")

# f_acyear <- parse_factor(c("2016/17", "2017/18", "2018/19"))

View(nature_of_work)

nature_of_work <- nature_of_work %>%
  mutate(study_mode = case_when(f_xqmode01 %in% ("Part-time") ~ 2 ,
                                   f_xqmode01 %in% ("Full-time") ~ 1 
  ))

nature_of_work <- nature_of_work %>%
  mutate(work_mean_num = case_when(f_wrkmean %in% ("Strongly agree") ~ 5,
                                   f_wrkmean %in% ("Agree")  ~ 4 ,
                                   f_wrkmean %in% ("Neither agree nor disagree") ~ 3 ,
                                   f_wrkmean %in% ("Disagree") ~ 2 ,
                                   f_wrkmean %in% ("Strongly disagree") ~ 1 
                                   ))

nature_of_work <- nature_of_work %>%
  mutate(work_skills_num = case_when(f_wrkskills %in% ("Strongly agree") ~ 5,
                                   f_wrkskills %in% ("Agree")  ~ 4 ,
                                   f_wrkskills %in% ("Neither agree nor disagree") ~ 3 ,
                                   f_wrkskills %in% ("Disagree") ~ 2 ,
                                   f_wrkskills %in% ("Strongly disagree") ~ 1 
                                   ))

nature_of_work <- nature_of_work %>%
  mutate(work_ontrack_num = case_when(f_wrkontrack %in% ("Strongly agree") ~ 5,
                                   f_wrkontrack %in% ("Agree")  ~ 4 ,
                                   f_wrkontrack %in% ("Neither agree nor disagree") ~ 3 ,
                                   f_wrkontrack %in% ("Disagree") ~ 2 ,
                                   f_wrkontrack %in% ("Strongly disagree") ~ 1 
                                    ))

nature_of_work <- nature_of_work %>%
  mutate(danow = ((work_ontrack_num + work_skills_num + work_mean_num) / 3))
          

#creating a data frame that containsonly distinct individuals
now_update2 <- distinct(nature_of_work, f_zanonymous, .keep_all = TRUE)

#restricting the data fram to those who studied full time and responded to all three GV questions
now_update2 <- subset(nature_of_work, study_mode == 1 | work_ontrack_num !=NA | work_skills_num!=NA | work_mean_num!=NA)


#creating a plot of fairwork score by SOC code and year

(gg2 <- ggplot(data = now_update2) +
  geom_col(mapping = aes(x = as.factor(f_zcohort),
                         y = mean(danow),
                         fill = f_xwrk2020soc1),
           position = position_dodge()
  ))
# ggplot(data = nature_of_work) +
#        geom_point(mapping = aes(x = f_acyear,
#                                 y = f_zreversedec)
#                   )    
(gg3 <- ggplot(data = now_update2) +
    geom_col(mapping = aes(x = as.factor(f_zcohort),
                           y = danow,
                           fill = f_xempbasis),
             position = position_dodge()
    ))

(gg4 <- ggplot(data = now_update2) +
    geom_col(mapping = aes(x = as.factor(f_zcohort),
                           y = danow,
                           fill = f_xglev501),
             position = position_dodge()
    ))
(gg5 <- ggplot(data = now_update2) +
    geom_col(mapping = aes(x = as.factor(f_zcohort),
                           y = danow,
                           fill = f_xethnic01),
             position = position_dodge()
    ))
#variables to consider plotting
#f_pared
#f_sexid
#f_xclass01
#f_xdomgr01
#f_ethnic01
#f_xglev501
#f_zstudis_marker
#f_xempbasis









