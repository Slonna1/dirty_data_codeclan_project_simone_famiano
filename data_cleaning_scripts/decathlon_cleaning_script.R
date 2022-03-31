library(tidyverse)
library(readr)
library(here)
library(janitor)
library(stringr)

#library I will be using

decathlon_data <-  read_rds(here("raw_data/decathlon.rds")) # reading the .rds file, storing it in a new variable: decathlon_data

decathlon_data

decathlon_data_clean <- clean_names(decathlon_data) #using the clean_names(function) to rename the column names to an acceptable standard

decathlon_data_clean <- decathlon_data_clean %>% #didn't do a great job, I'll rename them manually
  rename("100_meters" = x100m,
         "400_meters" = x400m,
         "110_meters_hurdle" = x110m_hurdle,
         "1500_meters" = x1500m)

#checking columns and rows

colnames(decathlon_data_clean)

rownames(decathlon_data_clean)

decathlon_data_clean <- decathlon_data_clean %>% 
  rownames_to_column("athlete") #giving a name to the column with athletes names

decathlon_data_clean <- decathlon_data_clean %>%
  mutate(competition = str_to_lower(competition)) #converting the characters in competition to lower case 

decathlon_data_clean_pivoted <- decathlon_data_clean %>% 
  pivot_longer(cols = !c("athlete", "competition"), 
               names_to = "sport_rank_points", values_to = "score" )

#pivoting all the columns expect for "athlete", "competition".
#names to "sport_rank_points", values to "score"

decathlon_data_clean_pivoted <- decathlon_data_clean_pivoted %>% 
  mutate(athlete = str_to_lower(athlete)) #converting the characters in athlete to lower case 

decathlon_data_clean_pivoted <- decathlon_data_clean_pivoted %>% 
  mutate(competition = str_replace(competition, "olympicg", "olympic_games" )) #changing to a more clear name

write_csv(decathlon_data_clean_pivoted, 
          "clean_data/decathlon_data_clean.csv")
write_rds(decathlon_data_clean_pivoted, 
          "clean_data/decathlon_data_clean.rds")

# writing the file in .csv and .rds