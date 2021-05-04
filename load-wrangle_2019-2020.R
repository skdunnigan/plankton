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


# 01-load-data ------------------------------------------------------------

dat <- readxl::read_xlsx(here::here('data', 'Plankton_2014-2020_skd.xlsx'),
                         sheet = 'RAWDATA_2014_2020') %>%
       janitor::clean_names() %>%
       mutate(tot_mag = tolower(tot_mag), # standardize entries
              vol = as.numeric(vol), # set volume col to numeric
              sp_code = as.character(sp_code), # set species codes to character, since identifier
             )

sp_codes <- readxl::read_xlsx(here::here('data', 'Plankton_2014-2020_skd.xlsx'),
                              sheet = 'species_codes') %>%
            janitor::clean_names() %>%
            rename(species_lu = species_description,
                   sp_code = code_for_vlookup, # using the code_for_vlookup for species codes
                   hi_lvl_code = hi_lvl_cat,
                   genus_code = general_species_cat,
                   genus_desc = general_species_desc) %>%
            mutate(sp_code = as.character(sp_code),
                   genus_code = as.character(genus_code),
                   hi_lvl_code = as.character(hi_lvl_code)
                   )


dplyr::glimpse(dat) # check dataframes
dplyr::glimpse(sp_codes) # check dataframes
janitor::get_dupes(dat) # check for duplicates
janitor::get_dupes(sp_codes) # check for duplicates

# 02-clean_2019-2020 ------------------------------------------------------

# remove unnecessary columns
# create month and year and month-year columns, and calculate time to identify
# filter out pre-2019 data, remove 400x magnification, only al_note of 0, qaqc of 0, and 4000 coded species
dat2 <- dat %>%
        select(-col_met, -id_name,
               -comment, -ent_date, -ent_name, -ent_notes,
               -checked_by, -checked_date, -check_comment) %>%
        mutate(year = year(col_date),
               month = month(col_date, label = TRUE),
               ym = paste0(year, '-', month),
               ID = paste0(sp_code, '_', col_date, '_', site),
               sample_ID_time = set_date - col_date,
              ) %>%
        filter(year >= 2019 & tot_mag != '400x' & al_notes == 0 & qaqc == 0 & sp_code < '4000')

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
                          select(sitedate, tot_sam_vol)

# view total sample volumes with how long it took for the sample to be identified
View(dat2 %>%
       select(site, ym, sample_ID_time, col_date) %>%
       distinct() %>%
       mutate(sitedate = paste0(site, col_date)) %>%
       select(-col_date) %>%
       left_join(total_sample_volumes, by = "sitedate") %>%
       select(-sitedate)
)

# 03-calculate-densities-per-sample-per-species ---------------------------

# sum total species counts for each of the samples
total_species_counts <- dat2 %>%
                          group_by(site, col_date, sp_code) %>%
                          summarise(sp_counts_sample = sum(total),
                                    count = n()) %>%  # counts are for if they were in multiple aliquots
                          ungroup() %>%
                          mutate(sitedate = paste0(site, col_date)) %>%
                          select(-count)

total_genus_counts <- dat2 %>%
                        left_join(sp_codes, by = "sp_code") %>%
                        group_by(site, col_date, genus_code) %>%
                        summarise(genus_counts_sample = sum(total),
                                  count = n()) %>%  # counts are for if they were in multiple aliquots
                        ungroup() %>%
                        mutate(sitedate = paste0(site, col_date)) %>%
                        select(-count)

total_hi_lvl_counts <- dat2 %>%
                        left_join(sp_codes, by = "sp_code") %>%
                        group_by(site, col_date, hi_lvl_code) %>%
                        summarise(hi_lvl_counts_sample = sum(total),
                                  count = n()) %>%  # counts are for if they were in multiple aliquots
                        ungroup() %>%
                        mutate(sitedate = paste0(site, col_date)) %>%
                        select(-count)

# merge total_sample_volumes with total_species_counts for density calculations
sp_density_per_samplevol <- left_join(total_species_counts, total_sample_volumes,
                                     by = "sitedate") %>%
                           mutate(sp_density = sp_counts_sample/tot_sam_vol,
                                  year = year(col_date),
                                  month = month(col_date, label = TRUE),
                                  ym = paste0(year, '-', month),
                                  ID = paste0(sp_code, '_', col_date, '_', site)) %>%
                           select(site, ym, sp_code, sp_counts_sample,
                                  tot_sam_vol, sp_density)

genus_density_per_samplevol <- left_join(total_genus_counts, total_sample_volumes,
                                        by = "sitedate") %>%
                               mutate(genus_density = genus_counts_sample/tot_sam_vol,
                                      year = year(col_date),
                                      month = month(col_date, label = TRUE),
                                      ym = paste0(year, '-', month),
                                      ID = paste0(genus_code, '_', col_date, '_', site)) %>%
                               select(site, ym, genus_code, genus_counts_sample,
                                      tot_sam_vol, genus_density)

hi_lvl_density_per_samplevol <- left_join(total_hi_lvl_counts, total_sample_volumes,
                                          by = "sitedate") %>%
                                mutate(hi_lvl_density = hi_lvl_counts_sample/tot_sam_vol,
                                       year = year(col_date),
                                       month = month(col_date, label = TRUE),
                                       ym = paste0(year, '-', month),
                                       ID = paste0(hi_lvl_code, '_', col_date, '_', site)) %>%
                                select(site, ym, hi_lvl_code, hi_lvl_counts_sample,
                                       tot_sam_vol, hi_lvl_density)



sp_den_primer <- sp_density_per_samplevol %>%
                  select(site, ym, sp_code, sp_density) %>%
                  tidyr::pivot_wider(names_from = sp_code,
                                     values_from = 'sp_density',
                                     id_cols = c("site", "ym"))

genus_den_primer <- genus_density_per_samplevol %>%
                    select(site, ym, genus_code, genus_density) %>%
                    tidyr::pivot_wider(names_from = genus_code,
                                       values_from = 'genus_density',
                                       id_cols = c("site", "ym"))

hi_lvl_den_primer <- hi_lvl_density_per_samplevol %>%
                     select(site, ym, hi_lvl_code, hi_lvl_density) %>%
                     tidyr::pivot_wider(names_from = hi_lvl_code,
                                        values_from = 'hi_lvl_density',
                                        id_cols = c("site", "ym"))

write.csv(sp_den_primer, here::here('output', 'data', 'sp_density_primer.csv'))
write.csv(genus_den_primer, here::here('output', 'data', 'genus_density_primer.csv'))
write.csv(hi_lvl_den_primer, here::here('output', 'data', 'hi_lvl_density_primer.csv'))
