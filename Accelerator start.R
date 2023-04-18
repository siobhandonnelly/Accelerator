#Installing the packages used to load in and clean the dataset
install.packages("tidyverse", dependencies = TRUE, type = "win.binary")

install.packages("readxl", dependencies = TRUE, type = "win.binary")

install.packages("janitor", dependencies = TRUE, type = "win.binary")
#Loading in the Tidyverse
library(tidyverse)
#Loading in the dataset
nature_of_work <- read_csv("UKHSA dataset.csv")
#Taking a look at the dataset tibble
nature_of_work
#Cleaning up the variable names
nature_of_work <- janitor::clean_names(nature_of_work)
#Starting to amend variable types. Changing the f_acyear variable to a factor

typeof("f_acyear")
parse_factor(c("2016/17", "2017/18", "2018/19"))

library(usethis)
?use_github
