library(tidyverse)
library(here)
library(stringr)
library(readxl)
library(janitor)

# libraries I will be using

candy_data_2017 <- read_xlsx(here("raw_data/boing-boing-candy-2017.xlsx"))

candy_data_2017

colnames(candy_data_2017) # checking column names

candy_data_2017_pivot_long <- candy_data_2017 %>% 
  pivot_longer(cols = c(7:109),
               names_to = "candy", values_to = "joy_or_despair")

#pivoting, storing all the candy names in a new column "candy".
#values to a new column: joy_or_despair 
#storing everything in a new variable: candy_data_2017_pivot_long

candy_data_2017_pivot_long <- candy_data_2017_pivot_long %>% 
  relocate(candy, .before = "Q1: GOING OUT?") %>% 
  relocate(joy_or_despair, .before = "Q1: GOING OUT?")

#relocating the two new columns on the front

colnames(candy_data_2017_pivot_long)

candy_data_2017_cleaned <- candy_data_2017_pivot_long %>% 
  rename(id = "Internal ID", going_out = "Q1: GOING OUT?", gender = 
           "Q2: GENDER",age = "Q3: AGE", country = "Q4: COUNTRY", 
         state_or_county = "Q5: STATE, PROVINCE, COUNTY, ETC", joy_other = 
           "Q7: JOY OTHER", despair_other = "Q8: DESPAIR OTHER")

#changing columns names to an acceptable standard
#storing in a new variable: candy_data_2017_cleaned

colnames(candy_data_2017_cleaned) # double-checking the new cleaned names

candy_data_2017_cleaned <-candy_data_2017_cleaned %>% 
  mutate(candy = str_remove_all(candy, "^Q[0-9][ ][|][ ]")) # removing unwanted characters from the candy column

candy_data_2017_cleaned %>% #checking if the names are tidy now.
  count(candy) %>% 
  view()

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  select(1:10) #just keeping the first 10 columns, the rest are not necessary in the Analysis questions

#candy_data_2017_cleaned <-candy_data_2017_cleaned %>% 
 # mutate(candy = str_remove_all(candy, "[\"]" )) #I'll come back to this if I need


candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_to_title(country)) #convert to capital all the first letter in each row

candy_data_2017_cleaned %>% #checking the country names (even worst than 2016!)
  count(country) %>% 
  view()

#starting cleaning them:

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_replace_all(country, "'Merica", "USA")) %>% 
  mutate(country = str_replace_all(country, "Ahem....Amerca", "USA")) %>% 
  mutate(country = str_replace_all(country, "Alaska", "USA")) %>% 
  mutate(country = str_replace_all(country, "America", "USA")) %>% 
  mutate(country = str_replace_all(country, "California", "USA")) %>% 
  mutate(country = str_replace_all(country, "Can", "Canada")) %>% 
  mutate(country = str_replace_all(country, "Canada`", "Canada")) %>% 
  mutate(country = str_replace_all(country, "Endland", "UK")) %>% 
  mutate(country = str_replace_all(country, "England", "UK")) %>% 
  mutate(country = str_replace_all(
    country, 
    "I Pretend To Be From Canada, But I Am Really From The United States.", 
    "USA")) %>% 
  mutate(country = str_replace_all(country, "Murica", "USA")) %>% 
  mutate(country = str_replace_all(country, "Murrika", "USA")) %>% 
  mutate(country = str_replace_all(country, "New Jersey", "USA")) %>% 
  mutate(country = str_replace_all(country, "New York", "USA")) %>% 
  mutate(country = str_replace_all(country, "North Carolina", "USA")) %>% 
  mutate(country = str_replace_all(country, "Pittsburgh", "USA")) %>% 
  mutate(country = str_replace_all(country, "Scotland", "UK")) %>% 
  mutate(country = str_replace_all(country, "The United States", "USA")) %>% 
  mutate(country = str_replace_all(country, "The United States Of America", 
                                   "USA")) %>% 
  mutate(country = str_replace_all(country, "Trumpistan", "USA")) %>% 
  mutate(country = str_replace_all(country, "U S", "USA")) %>%  
  mutate(country = str_replace_all(country, "U S A", "USA")) %>%  
  mutate(country = str_replace_all(country, "U.k.", "USA")) %>%  
  mutate(country = str_replace_all(country, "U.s.", "USA")) %>%  
  mutate(country = str_replace_all(country, "U.s.a.", "USA")) %>%  
  mutate(country = str_replace_all(country, "Uae", "UAE")) %>%  
  mutate(country = str_replace_all(country, "Uk", "UK")) %>%  
  mutate(country = str_replace_all(country, "Uk", "UK")) %>%  
  mutate(country = str_replace_all(country, "Unhinged States", "USA")) %>%  
  mutate(country = str_replace_all(country, "Unied States", "USA")) %>%  
  mutate(country = str_replace_all(country, "Unite States", "USA")) %>%  
  mutate(country = str_replace_all(country, "United Kingdom", "UK")) %>%  
  mutate(country = str_replace_all(country, "United Sates", "USA")) %>%  
  mutate(country = str_replace_all(country, "United Staes", "USA")) %>%  
  mutate(country = str_replace_all(country, "United State", "USA")) %>%  
  mutate(country = str_replace_all(country, "United Statea", "USA")) %>%  
  mutate(country = str_replace_all(country, "United Stated", "USA")) %>%  
  mutate(country = str_replace_all(country, "United States", "USA")) %>%  
  mutate(country = str_replace_all(country, "United States Of America", 
                                   "USA")) %>%  
  mutate(country = str_replace_all(country, "United Statss", "USA")) %>%  
  mutate(country = str_replace_all(country, "United Stste", "USA")) %>%  
  mutate(country = str_replace_all(country, "Unites States", "USA")) %>%  
  mutate(country = str_replace_all(country, "Us", "USA")) %>%    
  mutate(country = str_replace_all(country, "US", "USA")) %>%  
  mutate(country = str_replace_all(country, "US Of A", "USA")) %>% 
  mutate(country = str_replace_all(country, "Usa", "USA")) %>%  
  mutate(country = str_replace_all(country, "USa", "USA")) %>%  
  mutate(country = str_replace_all(country, "USA USA USA!!!!", "USA")) %>%  
  mutate(country = str_replace_all(country, "USA! USA! USA!", "USA")) %>%  
  mutate(country = str_replace_all(country, "USA? Hard To Tell Anymore..", 
                                   "USA")) %>%  
  
  mutate(country = str_replace_all(country, "USAA", "USA")) %>% 
  mutate(country = str_replace_all(country, "Usas", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAUSAUSA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USSA", "USA")) 

  

candy_data_2017_cleaned %>% #Checking if I missed something
  count(country) %>% 
  view()

#Yes I did:

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_replace_all(country, "Canadaada", "Canada")) %>% 
  mutate(country = str_replace_all(country, "Canadaada`", "Canada")) %>% 
  mutate(country = str_replace_all(country, "Canadaae", "Canada")) %>% 
  mutate(country = str_replace_all(country, "N. USA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA A", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA Of A", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA Of USA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAa", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAa USAa USAa!!!!", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA! USA! USA!", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA.", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAaa", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAas", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAausausa", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAd", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAa", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAs Of USA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA? Hard To Tell Anymore..", 
                                   "USA")) %>% 
  mutate(country = str_replace_all(
    country, "I Pretend To Be From Canadaada, But I Am Really From USA.", 
    "Canada")) 

candy_data_2017_cleaned %>% #checking again
  count(country) %>% 
  view()

#stil a few:

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_replace_all(country, "USAsausa", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAUSAUSA!!!", "USA")) %>% 
  mutate(country = str_replace_all(
    country,"I Pretend To Be From Canada, But I Am Really From USA", 
    "USA"))  

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_replace_all(country, "USA Of USA", "USA")) 

candy_data_2017_cleaned %>% 
  count(country) %>% 
  view()

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_replace_all(country, "Canada`", "Canada")) 

#checking the corresponding value in state_or_county column for the countries with an odd name to see if I can find a clue of where this person lives 
#If I can find anything I will convert the value in a NA.
# Odd countries name analysis:

cascadia_analysis <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Cascadia") # Cascadia for both country and state

#Cascadia is not a country, it's aregione that include part of the West Coast USA and Canada. I am going to leave this one in.

europe_analysis <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Europe") # America as state, alright. NA

candy_data_2017_cleaned <- candy_data_2017_cleaned %>%
  mutate(country = na_if(country, "Europe"))

fear_and_loathing <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Fear And Loathing") # not a real county or state, NA

candy_data_2017_cleaned <- candy_data_2017_cleaned %>%
  mutate(country = na_if(country, "Fear And Loathing"))

doesnt_know <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "I Don't Know Anymore") # North Carolina, to be included in USA

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_replace_all(country, "I Don't Know Anymore", "USA"))

insanity_lately <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Insanity Lately") #not a real county or state, NA

candy_data_2017_cleaned <- candy_data_2017_cleaned %>%
  mutate(country = na_if(country, "Insanity Lately"))

narnia_analysis <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Narnia") # Pennsylvania, to be included in USA

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_replace_all(country, "Narnia", "USA"))

soviet_canada <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Soviet Canadauckistan") # no state or county specified, I presume it's Canada

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_replace_all(country, "Soviet Canadauckistan", "Canada"))

subscribe_youtube <- soviet_canada <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Subscribe To Dm4uz3 On Youtube") #not a real county or state, NA

candy_data_2017_cleaned <- candy_data_2017_cleaned %>%
  mutate(country = na_if(country, "Subscribe To Dm4uz3 On Youtube"))


ud_analysis <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Ud") # NY, to be included in USA

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_replace_all(country, "Ud", "USA"))

atlantis_analysis <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "Atlantis")  # Petrolia*, to be included in Canada

#*city in Canada

candy_data_2017_cleaned <- candy_data_2017_cleaned %>% 
  mutate(country = str_replace_all(country, "Atlantis", "Canada"))

a_analysis <- candy_data_2017_cleaned %>% 
  select(country, state_or_county) %>% 
  filter(country == "A") # NA

candy_data_2017_cleaned <- candy_data_2017_cleaned %>%
  mutate(country = na_if(country, "A"))

candy_data_2017_cleaned <- candy_data_2017_cleaned %>%
  mutate(country = na_if(country, 1))

candy_data_2017_cleaned <- candy_data_2017_cleaned %>%
  mutate(country = na_if(country, 32))

candy_data_2017_cleaned <- candy_data_2017_cleaned %>%
  mutate(country = na_if(country, 45))

candy_data_2017_cleaned <- candy_data_2017_cleaned %>%
  mutate(country = na_if(country, 46))

candy_data_2017_cleaned <- candy_data_2017_cleaned %>%
  mutate(country = na_if(country, 35))

candy_data_2017_cleaned %>% 
  count(country) %>% 
  view()

colnames(candy_data_2017_cleaned) # one last check at the column names

write_csv(candy_data_2017_cleaned, file = 
            here("clean_data/candy_data_2017_cleaned.csv")) #writing te .csv file