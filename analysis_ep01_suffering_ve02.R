# Project name: Fact or legend ep01 - suffering, ve02
# Author: Bruno Alves de Carvalho
# Status: In Progress

# Set Up ------------------------------------------------------------------

# Set up the directory to the data warehouse
setwd("/Users/brunoalvesdecarvalho/Desktop/DataWarehouse_20231015_ve01")

# Load packages
library(tidyverse)
library(haven)

# Load functions from the warehouse
source("R_Scripts/FunctionRepository_20231016_ve01.R")

# Load color palette
source("R_Scripts/ColorPalette_20240128_ve01.R")

# Load data stored in the warehouse
merged_data_shp <-
  readRDS("SHP/Data_Aggregated_1999_2022/cached_mol_ed01.rds")


# Transform Data ----------------------------------------------------------

proper_var_names <- c(
  "rsl_ovrprbls", "rsl_chspssblt", "ctrl_dngevrthy", 
  "ctrl_fndsuccss", "ctrl_wnthnds", "ctrl_hppndpds", "ctrl_othrsdtrm", 
  "ctrl_pshdlfe", "ctrl_lttlinflf", "othrs_trst", "othrs_dontn", 
  "othrs_slintr", "othrs_vltr1", "othrs_vltr2", "gdthgs_jbst", 
  "gdthgs_rltshpst", "gdthgs_fnlst", "god_pryrs", "god_thkrlg", 
  "god_blfgd", "god_meditn", "god_flngone", "god_gdintrvns"
)

raw_var_names <- c(
  "p$$c72", "p$$c73", "p$$c104", "p$$c105", "p$$c106", 
  "p$$c107", "p$$c108", "p$$c109", "p$$c71", "p$$p45", "p$$n53", 
  "p$$n56", "p$$n35", "p$$n38", "p$$w228", "p$$ql04", "p$$i01", 
  "p$$r05", "p$$r15", "p$$r16", "p$$r17", "p$$r18", "p$$r19"
)

colnames(merged_data_shp)[colnames(merged_data_shp) %in% object02] <- 
  object

merged_data_shp <- 
  merged_data_shp %>% 
  mutate(rel_confession = 
           ifelse(`p$$r01` %in% c(1,2,3,4,9,10), 1,
                  ifelse(`p$$r01` < 0, NA, 0)),
         age_groups = 
           cut(`age$$`, 
               breaks = seq(25, 75, by = 5), 
               labels = c("25-30", "30-35", "35-40", "40-45", "45-50", "50-55",
                          "55-60", "60-65", "65-70", "70-75")))


transform_scales <- function(field) {
  ifelse(field < 0, NA, field)
}

tab_depression <-
  merged_data_shp %>% 
  mutate(depression_scale = 
           ifelse(`p$$c17` < 0, NA, `p$$c17`), 
         depression_scale_new = 
           depression_scale,
         across(
           all_of(object), 
           transform_scales)) %>% 
  group_by(idpers) %>% fill(depression_scale_new, .direction = "down") %>% 
  mutate(
    depressive_event = 
      ifelse(depression_scale_new < 0, NA, 
             ifelse(depression_scale_new > 8, 1, 0)), 
    cumsum_depressive_events = 
      cumsum(ifelse(is.na(depressive_event), 0, depressive_event)), 
    depression = 
      ifelse(cumsum_depressive_events >= 1, 1, depressive_event) %>% 
      factor(levels = c(0,1), labels = c("before", "after")), 
    sum_depressive_event = 
      sum(depressive_event, na.rm = T), 
    flag_1_depressive_event = 
      ifelse(sum_depressive_event > 1, 0, 1), 
    year_of_depressive_event = 
      ifelse(depressive_event == 1 & cumsum_depressive_events == 1, year, NA), 
    num_of_depressive_events = 
      ifelse(sum_depressive_event < 1, 0, 
             ifelse(sum_depressive_event == 1, 1, 
                    ifelse(sum_depressive_event > 1, 2, NA))) %>% 
      factor(levels = c(0,1,2), labels = c("none", "one", "more than one"))) %>% 
  fill(year_of_depressive_event, .direction = "updown") %>% 
  mutate(
    years_since_depressive_event = 
      year - year_of_depressive_event) %>% 
  ungroup() %>% 
  arrange(idpers, year)


# Exploratory Data Analysis -----------------------------------------------
tab_depression %>% 
  group_by(year, num_of_depressive_events) %>% 
  summarise(n = n(), mean_n = sum(!is.na(ctrl_hppndpds)), mean_ctrl_hppndpds = mean(ctrl_hppndpds, na.rm = T)) %>% 
  ggplot(aes(x = year, y = mean_ctrl_hppndpds, color = num_of_depressive_events)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = F) + 
  scale_y_continuous(limits = c(6,9))
tab_depression %>% 
  group_by(year, num_of_depressive_events) %>% 
  summarise(n = n(), mean_n = sum(!is.na(ctrl_lttlinflf)), mean_ctrl_lttlinflf = mean(ctrl_lttlinflf, na.rm = T)) %>% 
  filter(mean_n > 0) %>%
  ggplot(aes(x = year, y = mean_ctrl_lttlinflf, color = num_of_depressive_events)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)
tab_depression %>% 
  group_by(year, num_of_depressive_events) %>% 
  summarise(n = n(), mean_n = sum(!is.na(god_pryrs)), mean_god_pryrs = mean(god_pryrs, na.rm = T)) %>% 
  filter(mean_n > 0) %>% 
  ggplot(aes(x = year, y = mean_god_pryrs, color = num_of_depressive_events)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)


# Visualize Insights ------------------------------------------------------
tab_depression %>% 
  group_by(years_since_depressive_event, depression) %>% 
  summarise(n = n(), sample_size = sum(!is.na(ctrl_hppndpds)), mean_ctrl_hppndpds = mean(ctrl_hppndpds, na.rm = T)) %>%
  filter(!is.na(depression) & !is.na(years_since_depressive_event)) %>% 
  ggplot(aes(x = years_since_depressive_event, y = mean_ctrl_hppndpds)) + 
  geom_vline(xintercept = 0, linewidth = 0.75, color = grey) + 
  geom_point(aes(size = sample_size, alpha = sample_size), color = green) + 
  geom_smooth(se = F, span = 0.75, color = green, linewidth = 1.5) + 
  scale_x_continuous(limits = c(-10, 10)) + 
  geom_segment(aes(y = tab_depression %>%
                     filter(years_since_depressive_event < 0 & !is.na(ctrl_hppndpds)) %>% 
                     select(ctrl_hppndpds) %>% 
                     summarise(mean = mean(ctrl_hppndpds)) %>% 
                     unlist(), 
                   yend = tab_depression %>% 
                     filter(years_since_depressive_event < 0 & !is.na(ctrl_hppndpds)) %>% 
                     select(ctrl_hppndpds) %>% 
                     summarise(mean = mean(ctrl_hppndpds)) %>% 
                     unlist(), 
                   x = -10, xend = 0), 
               linewidth = 0.2, linetype = 2, color = grey) + 
  geom_segment(aes(y = tab_depression %>% 
                     filter(years_since_depressive_event > 0 & !is.na(ctrl_hppndpds)) %>% 
                     select(ctrl_hppndpds) %>% 
                     summarise(mean = mean(ctrl_hppndpds)) %>% 
                     unlist(), 
                   yend = tab_depression %>% 
                     filter(years_since_depressive_event > 0 & !is.na(ctrl_hppndpds)) %>% 
                     select(ctrl_hppndpds) %>% 
                     summarise(mean = mean(ctrl_hppndpds)) %>% 
                     unlist(), 
                   x = 0, xend = 10), 
               linewidth = 0.2, linetype = 2, color = grey) + 
  scale_y_continuous(limits = c(6.5, 8.5)) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal()



















