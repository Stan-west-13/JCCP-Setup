# http://langcog.github.io/wordbankr/
# http://wordbank.stanford.edu/
library(dplyr)
library(purrr)
library(stringr)
library(RMySQL)
remotes::install_github("langcog/wordbankr@9eabce62c42ced95fcae7fdf9bae3cf0d27e1ed5")
library(wordbankr)
library(DBI)
source("R/GetDataFromWordbank.R")
source("R/assign_percentiles.R")

## Host, dbname, and user can all be found within the MySQL server client by using the 'status' command.
# host and user can be found at 'Current user:'
# dbname can be found at 'Current database:'
# Your password will be whatever you use as your password to access the MySQL server. 


connection_info <- list(host="localhost",
                        dbname="wordbank",
                        user="root",
                        password="Shadowdog123!")


## Pull item-wise vocabulary data
CDI_WS <- GetDataFromWordbank(language = "English (American)", form = "WS",dbargs = connection_info) %>%
  mutate(num_item_id = as.numeric(str_extract(item_id, "\\d+")),
         form = "WS") 
CDI_WG <- GetDataFromWordbank(language = "English (American)", form = "WG",dbargs = connection_info) %>%
  mutate(form = "WG")

saveRDS(CDI_WS, file = paste0("CDI_WS_",Sys.Date(), ".rds"))
saveRDS(CDI_WG, file = paste0("CDI_WG_",Sys.Date(), ".rds"))



## Combine and clean files


mci_TDC_sentences <- CDI_WS %>%
  filter(type == 'word') %>%
  mutate(produced = (value == "produces")) %>%
  arrange(data_id)

mci_TDC_gestures <- CDI_WG%>%
  filter(type == 'word') %>%
  mutate(produced = (value == "produces")) %>%
  arrange(data_id)
############################################################

## 2. Loading productive vocabulary norms. ##############################################
vocabulary_norms_table_WG_CDI <- read.csv("words_produced_norms_table_WG_use.csv")
vocabulary_norms_table_WS_CDI <- read.csv("words_produced_norms_table_WS_use.csv")
############################################################

## Grouping NA into Typical and Late talkers above the 10th percentile ############
percent_thresh <- 10

lang_ability_WS <- assign_percentile_produces(mci_TDC_sentences, vocabulary_norms_table_WS_CDI, percent_thresh)
lang_ability_WG <- assign_percentile_produces(mci_TDC_gestures, vocabulary_norms_table_WG_CDI, percent_thresh)
## Counting TT vs LT
lang_ability_WS %>% group_by(group) %>% count()
lang_ability_WG %>% group_by(group) %>% count()
##################################################################


## Remove LT and add nProduced and add num_item_id and lemmas #########################33
cdi_metadata <- readRDS("cdi-metadata.rds")

mci_TDC_gestures_TD <- mci_TDC_gestures %>%
  select(-num_item_id) %>%
  filter(data_id %in% lang_ability_WG[lang_ability_WG$group == "TD",]$data_id) %>%
  group_by(data_id, age) %>%
  mutate(nProduced = sum(produced), form = "WG") %>%
  rename(
    subjectkey = data_id,
    interview_age = age,
    word = definition) %>%
  mutate(word = ifelse(word == "inside" | word == "in", "inside/in", word)) %>%
  left_join(., select(cdi_metadata,word,num_item_id,lemma), by = c("word"))

mci_TDC_sentences_TD <- mci_TDC_sentences %>%
  filter(data_id %in% lang_ability_WS[lang_ability_WS$group == "TD",]$data_id) %>%
  group_by(data_id, age) %>%
  mutate(nProduced = sum(produced), form = "WS") %>%
  rename(
    subjectkey = data_id,
    interview_age = age,
    word = definition) %>%
  left_join(., select(cdi_metadata,num_item_id,lemma), by = "num_item_id")
#######################################################################

## No repeats across lists and no kid with multiple entries.
sum(unique(mci_TDC_gestures_TD$subjectkey) %in% unique(mci_TDC_sentences_TD$subjectkey))

mci_TDC_gestures_TD %>%
  group_by(subjectkey) %>%
  mutate(rep_age = n_distinct(interview_age)>1) %>%
  filter(rep_age == TRUE)

mci_TDC_sentences_TD %>%
  group_by(subjectkey) %>%
  mutate(rep_age = n_distinct(interview_age)>1) %>%
  filter(rep_age == TRUE)
#########################################################

## Combine forms (no repeated subjectkeys between or within forms so I can just
## rbind.) and filter vocab size to between 20 and 600 words
mci_all <- rbind(mci_TDC_gestures_TD, mci_TDC_sentences_TD) %>%
  select(subjectkey, interview_age, sex, word, produced, nProduced, lemma, num_item_id, form) %>%
  filter(nProduced >= 20 & nProduced <= 600,
         !is.na(sex)) %>%
  mutate(group = "NA",
         sex = ifelse(sex == "Male" | sex == "M", "M", "F"))
##################################################################

save(mci_all, file = paste0("NA_all_long_",Sys.Date(),".Rdata"))













