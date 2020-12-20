library(tidyverse)
library(rvest)
library(countrycode)

# DATA --------------------------------------------------------------------


# Country ISO Codes -------------------------------------------------------

url <- "https://www.iban.com/country-codes"

temp <- url %>% 
  html %>%
  html_nodes("table")

spans <- temp %>% 
  html_nodes(xpath = "//*/tr/td/span")

xml_remove(spans)

ctry_codes <- html_table(temp[1], fill = TRUE)[[1]] %>% 
  janitor::clean_names()

# Democracy Index ---------------------------------------------------------


url <- "https://en.wikipedia.org/wiki/Democracy_Index"

temp <- url %>% 
  html %>%
  html_nodes("table")

DI_2019 <- html_table(temp[3])[[1]] %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  head(-1) %>% 
  mutate(rank = as.integer(rank)) %>% 
  mutate_at(vars(3:8), ~ as.numeric(.))
names(DI_2019) <- c("rank", 
                    "country", 
                    "score",                         
                    "electoral_process_and_pluralism",
                    "functioning_of_government",     
                    "political_participation",       
                    "political_culture", 
                    "civil_liberties", 
                    "regime_type",                    
                     "region", 
                     "changes_from_last_year")
DI_2019 <-  DI_2019 %>% 
  mutate(regime_type = factor(regime_type, levels = c("Authoritarian",
                                                      "Hybrid regime",
                                                      "Flawed democracy",
                                                      "Full democracy")),
         region = as.factor(region),
         country_code = countryname(country, destination = 'iso3c'))


# Per Capita PPA ----------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita"

temp <- url %>% 
  html %>%
  html_nodes("table")

spans <- temp %>% 
  html_nodes(xpath = "//*/tr/td/span")

xml_remove(spans)

PPA_FMI_2019 <- html_table(temp[3], fill = TRUE)[[1]] %>% 
  janitor::clean_names()
names(PPA_FMI_2019) <- c("rank_ppa", "country", "int_dollars")
PPA_FMI_2019 <- PPA_FMI_2019 %>% 
  mutate(country_code = countryname(country, destination = 'iso3c'))
PPA_FMI_2019 <- PPA_FMI_2019 %>% 
  mutate(int_dollars = str_remove(int_dollars, ",")) %>% 
  mutate(int_dollars = as.numeric(int_dollars))
PPA_BM_2016  <- html_table(temp[4], fill = TRUE)[[1]]
PPA_CIA_2017 <- html_table(temp[5], fill = TRUE)[[1]]



# System of Government ----------------------------------------------------


url <- "https://en.wikipedia.org/wiki/List_of_countries_by_system_of_government"

temp <- url %>% 
  html %>%
  html_nodes("table")

spans <- temp %>% 
  html_nodes(xpath = "//*/tr/td/span")

xml_remove(spans)

rep_mon <- html_table(temp[6], fill = TRUE)[[1]] %>% 
  janitor::clean_names()
names(rep_mon)[1:2] <- c("country", "const_form")
rep_mon <- rep_mon  %>% 
  mutate(country_code = countryname(country, destination = 'iso3c'))

# Gini Index --------------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_income_equality"

temp <- url %>% 
  html %>%
  html_nodes("table")

spans <- temp %>% 
  html_nodes(xpath = "//*/tr/td/span")

xml_remove(spans)

gini <- html_table(temp[4], fill = TRUE)[[1]] %>% 
  janitor::clean_names() %>% 
  select(country, world_bank_gini_4) %>% 
  tail(-1) %>% 
  as_tibble()
names(gini)[2] <- c("gini_index")
gini <- gini %>% 
  mutate(gini_index = as.numeric(gini_index)) %>% 
  mutate(country_code = countryname(country, destination = 'iso3c'))

# Freedom Index -----------------------------------------------------------

# Population --------------------------------------------------------------

pop <- world_bank_pop %>% 
  gather(year, pop, -(1:2)) %>% 
  spread(indicator, pop) %>% 
  filter(year == "2017") 


url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

temp <- url %>% 
  html %>%
  html_nodes("table")

spans <- temp %>% 
  html_nodes(xpath = "//*/tr/td/span")

xml_remove(spans)

pop <- html_table(temp[1], fill = TRUE)[[1]] %>% 
  janitor::clean_names() %>% 
  select(country = country_or_dependent_territory, 
         pop = population,
         per_pop = percent_of_world ) %>% 
  as_tibble() %>% 
  mutate(pop = as.numeric(str_remove_all(pop, ",")),
         per_pop = as.numeric(str_remove_all(per_pop, "%")),
         country = str_remove_all(country, "\\[.*\\]")) %>% 
  mutate(country_code = countryname(country, destination = 'iso3c'))

# Final Dataset -----------------------------------------------------------


all_data <- DI_2019 %>% 
  full_join(PPA_FMI_2019, by = "country_code") %>% 
  full_join(rep_mon, by = "country_code") %>% 
  full_join(gini, by = "country_code") %>%
  full_join(pop, by = "country_code") %>%
  select(country_code, score, regime_type, const_form, int_dollars, 
         gini_index,
         pop,
         region) %>% 
  drop_na() %>% 
  left_join(ctry_codes %>% select(country, alpha_3_code),
            by = c("country_code" = "alpha_3_code")) %>% 
  mutate(const_form = ifelse(str_detect(const_form, "monarchy"), 
                             "monarchy", const_form)) %>% 
  mutate(const_form = ifelse(const_form == "Republic", 
                             "republic", const_form)) %>% 
  filter(const_form != "n/a") %>% 
  mutate(const_form = factor(const_form))

write.csv2(all_data, here::here("data", "rep_mon.csv"), row.names = FALSE)

