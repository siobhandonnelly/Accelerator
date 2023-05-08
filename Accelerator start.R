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

nature_of_work$f_zcohort <- as.factor(nature_of_work$f_zcohort)

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


now_update3 <- now_update2 %>% 
  group_by(f_zcohort, f_xwrk2020soc1) %>% 
  mutate(mean_danow = mean(danow, na.rm = TRUE))


plot_creation <- function(df, colName) {
  df %>%
    group_by(f_zcohort, df[[colName]]) %>%
    mutate(mean_danow = mean(danow, na.rm = TRUE)) %>% 
    ggplot() +
    geom_col(
      mapping = aes(x = f_zcohort,
                    y = mean_danow,
                    fill = df[[colName]]),
      position = position_dodge()
    )
}

graph1 <- plot_creation(now_update2,"f_xwrk2020soc1")
graph1

graph2 <- plot_creation(now_update2,"f_xempbasis")
graph2

graph3 <- plot_creation(now_update2,"f_xglev501")
graph3

graph4 <- plot_creation(now_update2,"f_xclass01")
graph4

graph5 <- plot_creation(now_update2,"f_xdomgr01")
graph5



#variables to consider having as filters
#f_pared
#f_sexid
#instid
#f_ethnic01
#f_zstudis_marker





# outstanding issues 
# Rounding to 5  round_any(now_update3, 5) ?
#renaming columnnam
# chanigng colour
#reordering variables



