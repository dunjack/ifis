# Packages
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(gtable)
library(scales)
library(lubridate)
library(data.table)
library(tableone)
library(ggplot2)
library(ggthemes)
library(ggsignif)

###################################################
# 
d <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/191208_ifis.csv", header = TRUE, stringsAsFactors = FALSE))  %>%
  mutate(
    patkey = as.character(patkey),
    dob = dmy(dob),
    dat_op = dmy(dat_op),
    complication_bin = case_when(
      complication == "None" ~ "n",
      T ~ "y")
    ) %>%
  mutate(
    age = floor(interval( dob, dat_op)/ years(1))) %>%
  filter(op == "Phacoemulsification and IOL")


####################################################################################
library(tableone)

listVars <- c("gender", "age", "ethnicity", "eye",
              "phenylephrine", "iris_retract", "pupil_expan","high_cohesive", "complication_bin")

#"phenylephrine", "iris_retract", "pupil_expan","high_cohesive", "complication_bin")

catVars <- c("gender", "ethnicity", "eye",
             "phenylephrine", "iris_retract", "pupil_expan","high_cohesive", "complication_bin")

table1 <- 
  CreateTableOne(
    vars = listVars, 
    data = d,
    factorVars = catVars,
    strata = "complication"
    )

table1print <- print(table1, showAllLevels = TRUE, noSpaces = TRUE)
write.csv(table1print, file = "table_5.csv")

#strata = c("pupil_size", "cataract_w_b")
# , strata = "first_app_type"


dup <- d[duplicated(d$patkey),]

dup1 <- d %>% 
  semi_join(dup, by = "patkey") %>% 
  slice(which.min(dat_op)) %>%
  filter(!complication == "None")

