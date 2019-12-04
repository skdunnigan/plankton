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
codes <- codes %>%
  dplyr::mutate(sp_code = as.character(code)) %>%
  dplyr::select(-code)

# ----02 combine data so species description and group comes from 'codes'----
dat2 <- codes %>%
  dplyr::select(-notes, -code) %>% # remove unnecessary columns
  dplyr::right_join(dat, by = "sp_code") %>%
  dplyr::select(-col_met) # remove unnecessary columns

# ----03 calculate number of days between collection and identification----
dat2 <- dat2 %>% dplyr::mutate(proc_time = id_date - col_date)

# ----04 pull out single species counts and calculate density----
single_density <- dat2 %>%
  dplyr::filter(al_notes == 1) %>%
  dplyr::rename(total_count = total,
                total_volume = vol) %>%
  dplyr::mutate(density = total_count / total_volume) %>%
  dplyr::select(site, col_date, total_volume, sp_code, total_count, density)

# ----05 pull out diversity counts al_notes 0 & 2, pivot wider, add zeros----
diver_density <- dat2 %>%
  dplyr::filter(al_notes != 1) %>%
  dplyr::group_by(site, col_date, sp_code, al) %>%
  dplyr::summarise(total_volume = sum(vol),
                   total_count = sum(total)) %>%
  tidyr::pivot_wider(names_from = sp_code,
                     values_from = total_count,
                     id_cols = c("site", "col_date", "al", "total_volume")) %>%
  dplyr::ungroup()
# build in zeros into NAs
# this is to make sure that volumes of the full sample are used in the diversity counts, including species that were not counted in both aliquots (but present). Data sheets are only detecting and counting presence
diver_density[is.na(diver_density)] <- 0

# ----06 summarise everything in diversity, remove aliquot, pivot longer, calculate density----
diver_density2 <- diver_density %>%
  dplyr::group_by(site, col_date) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::select(-al) %>%
  tidyr::pivot_longer(-c(site, col_date, total_volume), #this keeps these columns in the data
                      names_to = "sp_code",
                      values_to = "total_count") %>%
  dplyr::mutate(density = total_count / total_volume)

# combine with single species
density <- bind_rows(diver_density2, single_density)






# density counts
  dplyr::group_by(site, col_date, sp_code) %>%
  dplyr::summarise(total_volume = sum(vol),
                   total_count = sum(total)) %>%
  dplyr::mutate(density = total_count / total_volume) %>%
  dplyr::left_join(codes, by = "sp_code") %>%
  dplyr::select(-notes)

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
