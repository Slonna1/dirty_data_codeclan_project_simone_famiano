library(tidyverse)
library(here)
library(stringr)
library(readxl)
library(janitor)

# libraries I will be using

candy_data_2016 <- read_xlsx(here("raw_data/boing-boing-candy-2016.xlsx"))

candy_data_2016

colnames(candy_data_2016) # checking column names

candy_data_2016_pivot_long <- candy_data_2016 %>% 
  pivot_longer(cols = c(7:106),
               names_to = "candy", values_to = "joy_or_despair")

#pivoting, storing all the candy names in a new column "candy".
#values to a new column: joy_or_despair 
#storing everything in a new variable: candy_data_2017_pivot_long

candy_data_2016_pivot_long <- candy_data_2016_pivot_long %>% 
  relocate(candy, .before = 
             `Are you going actually going trick or treating yourself?`) %>% 
  relocate(joy_or_despair, .after = candy)

#relocating the two new columns on the front of the table

colnames(candy_data_2016_pivot_long) #checking the columns

#as done with the 2015 data set, I am creating an id number for each person using the same process

candy_data_2016_cleaned  <- candy_data_2016_pivot_long %>% 
  mutate(Timestamp = str_remove_all(Timestamp, "[-][0-9][0-9]"))

candy_data_2016_cleaned  <- candy_data_2016_cleaned %>% 
  mutate(Timestamp = str_remove_all(Timestamp, "[:]"))

candy_data_2016_cleaned  <- candy_data_2016_cleaned %>% 
  mutate(Timestamp = str_remove_all(Timestamp, "[ ]"))

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  clean_names()

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  rename(id = timestamp, age = how_old_are_you , 
         going_out = are_you_going_actually_going_trick_or_treating_yourself,
         gender = your_gender, age = how_old_are_you, 
         country = which_country_do_you_live_in, 
         state_or_county = which_state_province_county_do_you_live_in)

#changing columns names to an acceptable standard
#storing in a new variable: candy_data_2017_cleaned

candy_data_2016_cleaned <-candy_data_2016_cleaned %>% 
  mutate(candy = str_remove_all(candy, "[\\[\\]]"))

# removing unwanted characters from the candy column

colnames(candy_data_2016_cleaned) #checking what columns I really need

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  select(1:8) # removing those which are not use for the analysis 

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  mutate(country = str_to_title(country)) #uppercase for all the rows in country

candy_data_2016_cleaned %>% #checking countries name
  count(country) %>% 
  view()

#(my God! I didn't know there were so many way to say USA)

#cleaning process:

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  mutate(country = str_replace_all(country, "America", "USA")) %>% 
  mutate(country = str_replace_all(country, "España", "Spain")) %>% 
  mutate(country = str_replace_all(country, "Merica", "USA")) %>% 
  mutate(country = str_replace_all(
    country, "Sub-Canadian North America... 'Merica", "USA")) %>% 
  mutate(country = str_replace_all(country, "The Best One - Usa", "USA")) %>% 
  mutate(country = str_replace_all(country, "Trumpistan", "USA")) %>% 
  mutate(country = str_replace_all(country, "The Yoo Ess Of Aaayyyyyy", 
                                   "USA")) %>% 
  mutate(country = str_replace_all(country, "U.s.", "USA")) %>% 
  mutate(country = str_replace_all(country, "U.s.a.", "USA")) %>% 
  mutate(country = str_replace_all(country, "Uk", "UK")) %>% 
  mutate(country = str_replace_all(country, "United  States Of America", 
                                   "USA")) %>% 
  mutate(country = str_replace_all(country, "United Kindom", "UK")) %>% 
  mutate(country = str_replace_all(country, "United Kingdom", "UK")) %>% 
  mutate(country = str_replace_all(country, "United States Of America", 
                                   "USA")) %>% 
  mutate(country = str_replace_all(country, "United Sates", "USA")) %>% 
  mutate(country = str_replace_all(country, "United States", "USA")) %>%
  mutate(country = str_replace_all(country, "United Stetes", "USA")) %>%
  mutate(country = str_replace_all(country, "Units States", "USA")) %>%
  mutate(country = str_replace_all(country, "Us", "USA")) %>%
  mutate(country = str_replace_all(country, "USAa", "USA")) %>%
  mutate(country = str_replace_all(country, "United State", "USA")) %>%
  mutate(country = str_replace_all(country, "USA.", "USA")) %>%
  mutate(country = str_replace_all(country, "United  States Of USA", 
                                   "USA")) %>%
  mutate(country = 
           str_replace_all(
             country,
             "USA(I Think But It's An Election Year So Who Can Really Tell)", 
             "USA")) %>%
  mutate(country = str_replace_all(country, "Usa Usa Usa", "USA")) %>%
  mutate(country = str_replace_all(country, "Usa Usa Usa Usa", "USA")) %>%
  mutate(country = str_replace_all(country, "Usa!", "USA")) %>%
  mutate(country = str_replace_all(country, "Usa! Usa!", "USA")) %>%
  mutate(country = str_replace_all(country, "Usa! Usa! Usa!", "USA")) %>%
  mutate(country = str_replace_all(country, "USA!!!!!", "USA")) %>%
  mutate(country = str_replace_all(country, "USA USA", "USA")) %>%
  mutate(country = str_replace_all(country, "USA USA USA", "USA")) %>%
  mutate(country = str_replace_all(country, "USAOf USA", "USA")) %>%
  mutate(country = str_replace_all(country, "USAUSAUSA", "USA")) %>%
  mutate(country = str_replace_all(country, "USAUSAUSAUSA", "USA")) %>%
  mutate(country = str_replace_all(country, "USA USA", "USA")) %>%
  mutate(country = str_replace_all(country, "USAUSA", "USA")) %>%
  mutate(country = str_replace_all(country, "Ussa", "USA"))  
  
candy_data_2016_cleaned %>% #check if I missed something 
  count(country) %>% 
  view()

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  mutate(country = str_remove_all(country, "[()]")) # removing special characters 

candy_data_2016_cleaned %>% 
  count(country) %>% 
  view()

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  mutate(country = 
           str_replace_all(
             country, 
             "USAI Think But It's An Election Year So Who Can Really Tell",
             "USA"))


candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  mutate(country = str_replace_all(country, "Eua", "EUA")) %>% 
  mutate(country = str_replace_all(country, "Murica", "USA"))  

#checking the corresponding value in state_or_county column for the countries with an odd name to see if I can find a clue of where this person lives 
#If I can find anuthing I will convert the value in a NA.
# Odd countries name analysis:

candy_data_2016_cleaned %>% 
  count(country) %>% 
  view()

cascadia_analysis <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Cascadia") # state Washington, to be included in USA

tropical_analysis <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "A Tropical Island South Of The Equator") # not a real county or state, NA

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "A Tropical Island South Of The Equator"))


denial_analysis <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Denial") # not a real county or state, NA

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "Denial"))

god_country_analysis <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "God's Country") # Douglass Commonwealth, aka Washington DC, to be included in USA

neverland_analysis <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Neverland") # not a real county or state, NA

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "Neverland"))

not_the_usa_or_canada <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Not The USAOr Canada") # not a real county or state, NA

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "Not The USAOr Canada"))

one_of_the_best <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "One Of The Best Ones") # not a real county or state, NA

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "One Of The Best Ones"))

see_above_analysis <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "See Above") # not a real county or state, NA

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "See Above"))

somewhere_analysis <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Somewhere") # not a real county or state, NA

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "Somewhere"))

republic_of_cascadia <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "The Republic Of Cascadia") # Cascachuan, Canadian province, to be added to Canada 

old_man_country <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "There Isn't One For Old Men") # Nothing, NA

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "There Isn't One For Old Men"))

this_one_analysis <- candy_data_2016_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "This One") # not a real county or state, NA

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "This One"))

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  mutate(country = str_replace_all(country, "Cascadia", "USA")) %>% 
  mutate(country = str_replace_all(country, "God's Country", "USA")) %>% 
  mutate(country = str_replace_all(country, "The Republic Of Cascadia", 
                                   "Canada")) 

#notice I've missed some countries names before:

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  mutate(country = str_replace_all(country, "The Republic Of USA", "USA"))

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  mutate(country = str_replace_all(country, "Sub-Canadian North USA.. 'USA", 
                                   "USA"))

candy_data_2016_cleaned <- candy_data_2016_cleaned %>% 
  mutate(country = str_replace_all(country, "England", "UK"))

#converting the rest of odd countries names to NAs 

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "30.0"))

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "44.0"))

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "45.0"))

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "47.0"))

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "51.0"))

candy_data_2016_cleaned <- candy_data_2016_cleaned %>%
  mutate(country = na_if(country, "54.0"))

candy_data_2016_cleaned %>% 
  count(country) %>% 
  view()

#one last check before writing the file

write_csv(candy_data_2016_cleaned, file = 
            here("clean_data/candy_data_2016_cleaned.csv"))

#writing the file