# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  3/19/2024
## Date Edited:   12/8/2025


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, nccsdata)

# Import data -------------------------------------------------------------
source("data-code/build-irs-raw.R")

# Merge EINs --------------------------------------------------------------
ein.aha <- read_rds("data/input/microdata/ein-aha-crosswalk/AHA_ein_matches.rds")
ein.aha.manual <- read_rds("data/input/microdata/ein-aha-crosswalk/manual_matched_eins.rds") %>%
    mutate(ID = as.numeric(ID), ein_hosp2 = as.numeric(ein_hosp), ein_sys2=as.numeric(ein_sys)) %>%
    select(ID, ein_hosp2, ein_sys2)

ein.aha <- ein.aha %>% left_join(ein.aha.manual, by="ID") %>%
    mutate(ein_hosp = coalesce(ein_hosp, ein_hosp2), ein_sys = coalesce(ein_sys, ein_sys2)) %>%
    select(-ein_hosp2, -ein_sys2)

ein.aha.long <- ein.aha %>% pivot_longer(cols=c(ein_hosp, ein_sys), names_to="type", values_to="ein") %>%
    filter(!is.na(ein)) %>% select(ID, type, ein) %>% mutate(type = if_else(type == "ein_hosp", "hospital", "system")) %>%
    group_by(ein, type) %>% mutate(rcount=row_number()) %>% ungroup()

ein.aha.wide <- ein.aha.long %>% pivot_wider(values_from="ID", names_from=c("type", "rcount"), names_prefix="ID_") %>%
    select(ein, starts_with("ID_hospital"), starts_with("ID_system"))

irs.dat.aha <- final.tax.dat %>% mutate(ein=as.numeric(ein)) %>%
    left_join(ein.aha.wide, by = c("ein"), relationship="many-to-one") 

# Export data -------------------------------------------------------------

write_tsv(irs.dat.aha,'data/output/form990_ahaid.txt')
