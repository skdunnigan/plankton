library(tidyverse)
library(here)
library(janitor)
library(readxl)

# ----01a load in data----
dat <- readxl::read_xlsx(here::here('data', 'GTMNERR_Plankton_IK.xlsx'), sheet = "raw") %>%
  janitor::clean_names()
codes <- readxl::read_xlsx(here::here('data', 'GTMPlankton_SpeciesCodes_IK.xlsx'), sheet = "SpeciesCodes") %>%
  janitor::clean_names()

# ----01b make sure species codes are read as characters and not numeric----
dat <- dat %>% dplyr::mutate(sp_code = as.character(sp_code))
codes <- codes %>%   mutate(sp_code = as.character(code))

# ----02 combine data so species description and group comes from 'codes'----
dat2 <- codes %>%
  select(-notes, -code) %>%
  right_join(dat, by = "sp_code") %>%
  select(-col_met) # remove unnecessary columns

# ----03 calculate number of days between collection and identification----
dat2 <- dat2 %>% dplyr::mutate(proc_time = id_date - col_date)

# ----04 pull out single species counts and calculate density----
single_density <- dat2 %>%
  dplyr::filter(al_notes == 1) %>%
  dplyr::mutate(density = total / vol)

# ----05 pull out diversity counts al_notes 0 & 2
density_dat <- dat %>%
  mutate(sp_code2 = as.character(sp_code)) %>%
  group_by(site, col_date, sp_code2) %>%
  summarise(total_volume = sum(vol),
            total_count = sum(total)) %>%
  mutate(density = total_count / total_volume)

# make wider to build in zeros
dat2 <- dat %>%
  mutate(sp_code2 = as.character(sp_code)) %>%
  group_by(site, col_date, al, vol, sp_code2) %>%
  summarise(tot_vol = sum(vol),
            tot_count = sum(total)) %>%
  tidyr::pivot_wider(names_from = sp_code2,
                     values_from = tot_count,
                     id_cols = c("site", "col_date", "al", "vol", "tot_vol"))
dat3 <- dat2
dat3[is.na(dat3)] <- 0

dat3 %>%
  group_by()



density_dat <- dat %>%
  mutate(sp_code2 = as.character(sp_code)) %>%
  group_by(site, col_date, sp_code2) %>%
  summarise(total_volume = sum(vol),
            total_count = sum(total)) %>%
  mutate(density = total_count / total_volume)

density_dat %>%
  tidyr::pivot_wider(id_cols = c("site", "col_date"),
                     names_from = sp_code2,
                     values_from = density)
