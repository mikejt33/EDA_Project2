##Script to clean data

library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)

##Assuming you are using the GitHub directory as your starting point


##Dropping all na values 
data <- read_csv("Data/EDA_Data.csv", col_names = TRUE) %>%
  filter(count != "na") %>%
  mutate(counts = as.numeric(count)) %>%
  select(-(count))