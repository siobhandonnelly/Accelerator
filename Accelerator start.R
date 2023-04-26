#Installing the packages used to load in and clean the dataset
install.packages("tidyverse", dependencies = TRUE, type = "win.binary")

install.packages("readxl", dependencies = TRUE, type = "win.binary")

install.packages("janitor", dependencies = TRUE, type = "win.binary")
#Loading in the Tidyverse
library(tidyverse)
#Loading in the dataset
#trying to load in data with nulls de\lt with but code not working
nature_of_work <- read_csv("UKHSA dataset.csv")
#nature_of_work <- read_csv("UKHSA dataset.csv"), 
#                           na = c("U", "Unknown (IMD)", "Not applicable (IMD)", "Not applicable (NIMMD)", "Unknown (NIMDM)" ,"Not applicable (SIMD)" ,"Unknown (SIMD)", "Not applicable (WIMD)", "Unknown (WIMD)","Z0 - Missing data" ,"NA" , "Not known, "'Not known/Not applicable'", "'Not applicable'", "'03'", "'-1'","'*'", ".", "", "'NULL'"))

#Taking a look at the dataset tibble
nature_of_work
#Cleaning up the variable names
nature_of_work <- janitor::clean_names(nature_of_work)
#Starting to amend variable types. Changing the f_acyear variable to a factor

typeof("f_acyear")
parse_factor(c("2016/17", "2017/18", "2018/19"))

library(usethis)
?use_github

use_github(protocol = 'https', auth_token = Sys.getenv("GITHUB_PAT"))
View(nature_of_work)


library(ggplot2)

n_distinct(f_acyear)

ggplot(data = nature_of_work) +
       geom_point(mapping = aes(x = f_acyear,
                                y = f_zcohort)
                  )


ggplot(data = nature_of_work) +
       geom_point(mapping = aes(x = f_acyear,
                                y = f_zreversedec)
                  )
