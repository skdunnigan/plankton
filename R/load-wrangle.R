# install.packages(c('tidyverse', 'janitor', 'readxl', 'here',
#                    'tidyr', 'dplyr', 'gganimate', 'ggpubr'))

library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(tidyr)
library(dplyr)
library(lubridate)
library(gganimate)
library(ggpubr)


# load-data ---------------------------------------------------------------

dat <- readxl::read_xlsx(here::here('data', 'SWMP_Plankton_2020_SrayPRIMERXFORM_042621v2.xlsx'),
                         sheet = 'RawData-2020') %>%
      janitor::clean_names()

codes <- readxl::read_xlsx(here::here('data', 'SWMP_Plankton_2020_SrayPRIMERXFORM_042621v2.xlsx'),
                           sheet = 'species_codes_skd') %>%
        janitor::clean_names()

dplyr::glimpse(dat)

# check columns
unique(dat$vol)
## need to remove '??' entry from `vol` and then reassign to numeric
unique(dat$tot_mag)
## need to lower 'x' in magnification entries

dat2 <- dat %>%
        select(-set_date, -col_met, -id_date, -id_name) %>%
        filter(vol != '??' & tot_mag != '400x') %>% # remove '??' volumes and '400x' entries
        mutate(tot_mag = tolower(tot_mag), # standardize entries
               vol = as.numeric(vol), # set volume col to numeric
               sp_code = as.character(sp_code), # set species codes to character, since identifier
               ) %>%
        filter(al_notes == 0) # keep the al_notes with `0`, these are diversity counts


# calculate densities -----------------------------------------------------

# get total sample volume for every site-collection_date
total_sample_volumes <- dat2 %>%
                        group_by(site, col_date, al, vol) %>%
                        summarise(count = n()) %>%
                        select(-count) %>%
                        ungroup() %>%
                        group_by(site, col_date) %>%
                        summarise(tot_sam_vol = sum(vol),
                                  count = n()) %>%
                        ungroup() %>%
  mutate(sitedate = paste0(site, col_date)) %>%
  select(-count)

# sum total species counts for each of the samples
total_species_counts <- dat2 %>%
  group_by(site, col_date, sp_code) %>%
  summarise(sp_counts_sample = sum(total),
            count = n()) %>%  # counts are for if they were in multiple aliquots
  ungroup() %>%
  mutate(sitedate = paste0(site, col_date)) %>%
  select(-count)

# merge total_sample_volumes with total_species_counts for density calculations
sp_counts_per_samplevol <- left_join(total_species_counts, total_sample_volumes,
                                     by = "sitedate") %>%
  select(-site.y, -col_date.y) %>%
  rename(site = site.x,
         col_date = col_date.x)

sp_den_per_sample <- sp_counts_per_samplevol %>%
  mutate(density = sp_counts_sample/tot_sam_vol,
         year = year(col_date),
         month = month(col_date, label = TRUE),
         ym = paste0(year, '-', month),
         ID = paste0(sp_code, '_', col_date, '_', site))

den_primer <- sp_den_per_sample %>%
  select(site, ym, sp_code, density) %>%
  tidyr::pivot_wider(names_from = sp_code,
                     values_from = 'density')

# replace all NAs with blanks
# this is a new dataframe because it will make everything factors
# this is JUST to export the data into a csv without NAs
export <- sapply(den_primer, as.character)
export[is.na(export)] <- " "
export <- as.data.frame(export)
write.csv(export, here::here('output', 'data', 'densities_species.csv'))

code_ready <- codes %>%
  select(code, species_description, hi_lvl_cat, hi_lvl_desc) %>%
  rename(sp_code = code,
         sp_desc = species_description,
         hi_lvl_code = hi_lvl_cat) %>%
  mutate(sp_code = as.character(sp_code),
         hi_lvl_code = as.character(hi_lvl_code))

code_flip <- as.data.frame(t(code_ready)) %>%
  row_to_names(row_number = 1) %>%
  write_csv(here::here('output', 'data', 'tags.csv'))
