#Installing the packages used to load in and clean the dataset
# install.packages("tidyverse", dependencies = TRUE, type = "win.binary")
# install.packages("readxl", dependencies = TRUE, type = "win.binary")
# install.packages("janitor", dependencies = TRUE, type = "win.binary")
# install.packages("plotly", dependencies = TRUE, type = "win.binary")

#Loading in the Tidyverse
library(tidyverse)
library(usethis)
library(ggplot2)
library(dplyr)
library(plotly)
?use_github

# use_github(protocol = 'https', auth_token = Sys.getenv("GITHUB_PAT"))



#Trying to load in data with nulls dealt with
nature_of_work <- read_csv("UKHSA dataset.csv", na = c("U", "Z0 - Missing data" ,"NA", "Not Known" ,"Not applicable", "-1","*",".", "", "NULL"))
#Taking a look at the dataset 
View(nature_of_work)

nature_of_work$f_zcohort <- as.factor(nature_of_work$f_zcohort)

#Attempting to order the SOCcode variable
typeof("f_xwrk2020soc1")

nature_of_work$f_xwrk2020soc1 <- factor(nature_of_work$f_xwrk2020soc1, levels = c("Managers, directors and senior officials", "Professional occupations", "Associate professional and technical occupations", "Administrative and secretarial occupations", "Skilled trades occupations", "Caring, leisure and other service occupations", "Sales and customer service occupations", "Process plant and machine operatives", "Elementary occupations"))



nature_of_work$f_xempbasis <- factor(nature_of_work$f_xempbasis, levels = c("On a permanent/open ended contract", "On a fixed-term contract lasting 12 months or longer","On a fixed-term contract lasting less than 12 months","Temping (including supply teaching)","On a zero hours contract","Volunteering","On an internship","Other","Not known"))



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



plot_creation <- function(df, colName) {
 ndf <- df %>%
    filter(!is.na(df[[colName]])) 
 ndf %>% 
    group_by(f_zcohort, ndf[[colName]]) %>%
    mutate(mean_danow = mean(danow, na.rm = TRUE)) %>% 
    ggplot() +
    geom_col(
      mapping = aes(x = f_zcohort,
                    y = mean_danow,
                    fill = ndf[[colName]]),
      position = position_dodge(),
    ) +
        coord_cartesian(ylim=c(1,5)) +
        scale_fill_manual(values = c("#1F4388", 
                                 "#83C7BC", 
                                 "#1E355E",
                                 "#6A86B8",
                                 "#A93439",
                                 "#CE3162",
                                 "#E57D3A",
                                 "#4EA585",
                                 "#BBB332",
                                 "#E8D77E")
    ) # adding HESA colours
}


# Chart 1: Showing the design and nature of work score by the graduates Standard occupational code
graph1 <- plot_creation(now_update2,"f_xwrk2020soc1") + 

  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Standard Occupational Code", # Legend Label
       title = "Graduates Design and nature of work score by SOC group",
       subtitle = "",
       caption = "")

graph1


# Chart 2: Showing the design and nature of work score by the graduates employment basis

graph2 <- plot_creation(now_update2,"f_xempbasis") +
  
  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Employment Basis", # Legend Label
       title = "Graduates Design and nature of work score by Employment Basis",
       subtitle = "",
       caption = "")
graph2


# Chart 3: Showing the design and nature of work score by the graduates level of study

graph3 <- plot_creation(now_update2,"f_xglev501") +
  
  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Level of Study", # Legend Label
       title = "Graduates Design and nature of work score by Level of Study",
       subtitle = "",
       caption = "")
graph3


# Chart 4: Showing the design and nature of work score by the graduates class of degree attained

graph4 <- plot_creation(now_update2,"f_xclass01") +
  
  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Class of Degree", # Legend Label
       title = "Graduates Design and nature of work score by Class of Degree",
       subtitle = "",
       caption = "")
graph4


# Chart 5: Showing the design and nature of work score by the graduates domicile

graph5 <- plot_creation(now_update2,"f_xdomgr01") +
  
  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Employment Basis", # Legend Label
       title = "Graduates Design and nature of work score by Employment Basis",
       subtitle = "",
       caption = "")
graph5


#creating an interactive plot for graph1
ggplotly(graph1)


#working to try and replicate the above function but with creating a plot_ly chart. The code is not working yet. This is to see if there is a difference in speed between the two options

plotly_creation <- function(df, colName) {
  ndf <- df %>%
    filter(!is.na(df[[colName]])) 
  ndf %>% 
    group_by(f_zcohort, ndf[[colName]]) %>%
    summarize(mean_danow = mean(danow, na.rm = TRUE)) %>% 
    plot_ly(
      x = ~f_zcohort,
      y = ~mean_danow,
      color = ~ndf[[colName]],
      type = "bar"
     ) %>%
     layout(
       yaxis = list(range = c(1, 5)),
       colorway = c("#1F4388", "#83C7BC", "#1E355E", "#6A86B8", "#A93439", "#CE3162", "#E57D3A", "#4EA585", "#BBB332", "#E8D77E")
     )
}
Chart1 <- plotly_creation(now_update2,"f_xwrk2020soc1") 

