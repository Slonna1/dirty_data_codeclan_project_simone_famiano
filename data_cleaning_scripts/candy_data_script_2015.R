library(tidyverse)
library(here)
library(stringr)
library(readxl)
library(janitor)

#lbrary I will be using.

candy_data_2015 <- read_xlsx(here("raw_data/boing-boing-candy-2015.xlsx"))

candy_data_2015

colnames(candy_data_2015)

candy_data_2015_pivot_long <- candy_data_2015 %>% 
  pivot_longer(cols = c(4:96), 
               names_to = "candy", values_to = "joy_or_despair" )

candy_data_2015_pivot_long <- candy_data_2015_pivot_long %>% 
  relocate(candy, .before = `How old are you?`) %>% 
  relocate(joy_or_despair, .before = `How old are you?`)

#pivoting, storing all the candy names in a new column "candy".
#values to a new column: joy_or_despair 
#storing everything in a new variable: candy_data_2015_pivot_long

colnames(candy_data_2015_pivot_long) # checking the new columns 


#I have noticed than the 2017 data set doesn't have a date and time entry but rather an ID for the people surveyed
#It will be best to have an id column for all the data sets
#I will create one now by removing any special character from the date and time, I will keep the year (2015) and the time (ex. 08:00:00) united in 
#a unique id number

#I am going to used mutate() and str_remove_all, along with some REGEX

candy_data_2015_cleaned  <- candy_data_2015_pivot_long %>% 
  mutate(Timestamp = str_remove_all(Timestamp, "[-][0-9][0-9]"))

candy_data_2015_cleaned  <- candy_data_2015_cleaned %>% 
  mutate(Timestamp = str_remove_all(Timestamp, "[:]"))

candy_data_2015_cleaned  <- candy_data_2015_cleaned %>% 
  mutate(Timestamp = str_remove_all(Timestamp, "[ ]"))


candy_data_2015_cleaned <- candy_data_2015_cleaned %>% #a run with Janitor function clean_names() to tidy column names
  clean_names()

colnames(candy_data_2015_cleaned)

#renaming the columns to shorter names
#I will be using these names for the other two data sets as well

candy_data_2015_cleaned <- candy_data_2015_cleaned %>% 
  rename( id = timestamp, age = how_old_are_you, 
          going_out = are_you_going_actually_going_trick_or_treating_yourself,
          joy_other = 
            please_list_any_items_not_included_above_that_give_you_joy,
          despair_other = 
            please_list_any_items_not_included_above_that_give_you_despair,
          )

colnames(candy_data_2015_cleaned) #checking if all looks alright 

candy_data_2015_cleaned <-candy_data_2015_cleaned %>% 
  mutate(candy = str_remove_all(candy, "[\\[\\]]")) #removing any spacial character from the candy columns


candy_data_2015_cleaned <- candy_data_2015_cleaned %>% 
  select(c(1:5,7,8)) 

#Just keeping the columns necessary for the Analysis Questions

colnames(candy_data_2015_cleaned) # one last check


write_csv(candy_data_2015_cleaned, file = here(
  "clean_data/candy_data_2015_cleaned.csv"))

#writing the .csv file