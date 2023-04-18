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