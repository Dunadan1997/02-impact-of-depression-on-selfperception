# Title: Myth or Legend? 1st edition special
# Author: Bruno Alves de Carvalho

# Setting the directory to the data warehouse
setwd("/Users/brunoalvesdecarvalho/Desktop/DataWarehouse_20231015_ve01")

# Loading packages
library(tidyverse)
library(haven)

# Loading functions from the warehouse
source("R_Scripts/FunctionRepository_20231016_ve01.R")

# Loading data stored in the warehouse
merged_data_shp <-
  readRDS("SHP/Data_Aggregated_1999_2022/cached_mol_ed01.rds")

# Creating event variables for suffering

  ## Defining "serious" depressive event as >8 and removing individuals with
  ## more than 1 depressive event. So we're only considering people with a single
  ## recording of depression
depression_01 <- 
  merged_data_shp %>% 
  mutate(depression = ifelse(`p$$c17` > 8, 1, 0)) %>% 
  group_by(idpers) %>% 
  summarise(total_depression_events = sum(depression)) %>%
  filter(total_depression_events == 1) %>% 
  left_join(merged_data_shp, by = "idpers") %>% 
  mutate(depression = ifelse(`p$$c17` > 8, 1, 0))

  ## Defining range of study by identifying the start and end year of each individual
depression_02 <- 
  depression_01 %>% 
  group_by(idpers) %>% 
  summarise(first_year = min(year), last_year = max(year)) %>% 
  left_join(depression_01, by = "idpers")

  ## Removing individuals for which the depressive event occurred at the start 
  ## or end of the range of study (i.e. for which there is no data for after or before
  ## the depressive event)
depression_03 <- 
  depression_02 %>% 
  select(idpers, year, depression, first_year, last_year) %>% 
  filter(depression == 1) %>% 
  mutate(depression_year = year) %>% 
  filter(depression_year != first_year & depression_year != last_year) %>% 
  select(idpers, depression_year) %>% 
  left_join(depression_02, by = "idpers")

  ## Nesting data in order to slice the range of study in two: before and after 
  ## depression, for each individual. Processing variables too.
depression_04 <- 
  depression_03 %>% 
  mutate(idpers_02 = idpers,
         rel_confession = ifelse(`p$$r01` %in% c(1,2,3,4,9,10), 1, 
                                 ifelse(`p$$r01` < 0, NA, 0)),
         rsl_ovrprbls = ifelse(`p$$c72` < 0, NA, `p$$c72`),
         rsl_chspssblt = ifelse(`p$$c73` < 0, NA, `p$$c73`),
         ctrl_dngevrthy = ifelse(`p$$c104` < 0, NA, `p$$c104`),
         ctrl_fndsuccss = ifelse(`p$$c105` < 0, NA, `p$$c105`),
         ctrl_wnthnds = ifelse(`p$$c106` < 0, NA, `p$$c106`),
         ctrl_hppndpds = ifelse(`p$$c107` < 0, NA, `p$$c107`),
         ctrl_othrsdtrm = ifelse(`p$$c108` < 0, NA, `p$$c108`),
         ctrl_pshdlfe = ifelse(`p$$c109` < 0, NA, `p$$c109`),
         ctrl_lttlinflf = ifelse(`p$$c71` < 0, NA, `p$$c71`),
         othrs_trst = ifelse(`p$$p45` < 0, NA, `p$$p45`),
         othrs_dontn = ifelse(`p$$n53` < 0, NA, `p$$n53`),
         othrs_slintr = ifelse(`p$$n56` < 0, NA, `p$$n56`),
         othrs_vltr1 = ifelse(`p$$n35` < 0, NA, `p$$n35`),
         othrs_vltr2 = ifelse(`p$$n38` < 0, NA, `p$$n38`),
         gdthgs_jbst = ifelse(`p$$w228` < 0, NA, `p$$w228`),
         gdthgs_rltshpst = ifelse(`p$$ql04` < 0, NA, `p$$ql04`),
         gdthgs_fnlst = ifelse(`p$$i01` < 0, NA, `p$$i01`),
         god_pryrs = ifelse(`p$$r05` < 0, NA, `p$$r05`),
         god_thkrlg = ifelse(`p$$r15` < 0, NA, `p$$r15`),
         god_blfgd = ifelse(`p$$r16` < 0, NA, `p$$r16`),
         god_meditn = ifelse(`p$$r17` < 0, NA, `p$$r17`),
         god_flngone = ifelse(`p$$r18` < 0, NA, `p$$r18`),
         god_gdintrvns = ifelse(`p$$r19` < 0, NA, `p$$r19`)
         ) %>% 
  nest_by(idpers_02)

    ## Slicing data into before and after depression, for each individual seperately
before_depression <- 
  map(depression_04$data, function(.) subset(., year < depression_year))
after_depression <-
  map(depression_04$data, function(.) subset(., year >= depression_year))

  ## Selecting variables needed for hypotheses testing
var_names <- 
  depression_04 %>% 
  pluck(2, 1) %>% 
  select(starts_with(c("rsl", "ctrl", "othrs", "gdthgs", "god")), rel_confession, idpers) %>% 
  colnames()

x_01 <- 
  NULL

for (i in seq_along(var_names)) {
  
  ## Calculating the average score for every variable of each individual before 
  ## suffering from depression
  x_01 <- 
    before_depression %>% 
    map(var_names[i]) %>% 
    map(mean, na.rm = T) %>% 
    unlist() %>% 
    na_if(NaN) %>% 
    tibble() %>% 
    bind_cols(x_01)
  
  ## Renaming columns
  colnames(x_01) <- 
    paste0(var_names[i:1], "_before")
  
}

x_01 <- 
  x_01 %>% 
  rowid_to_column(var = "rowid") %>% 
  rename(idpers = idpers_before)

y_01 <- 
  NULL

for (i in seq_along(var_names)) {
  
  ## Calculating the average score for every variable of each individual after 
  ## suffering from depression
  y_01 <- 
    after_depression %>% 
    map(var_names[i]) %>% 
    map(mean, na.rm = T) %>% 
    unlist() %>% 
    na_if(NaN) %>% 
    tibble() %>% 
    bind_cols(y_01)
  
  ## Renaming columns
  colnames(y_01) <- 
    paste0(var_names[i:1], "_after")
  
}

y_01 <- 
  y_01 %>% 
  rowid_to_column(var = "rowid") %>% 
  rename(idpers = idpers_after)
  
  ## Creating FINAL data set, ready for test hypotheses
mol_ed01_special_data <- 
  left_join(x_01, y_01, by = "idpers")

mol_ed01_special_christian_data <-
  left_join(
    subset(x_01, rel_confession_before == 1), 
    subset(y_01, rel_confession_after == 1),
    by = "idpers")

# Exploring data
x1 <- 
  depression_04 %>% 
  unnest(cols = "data")

x1 %>%  
  select(idpers, `sex$$`) %>% 
  distinct() %>% group_by(`sex$$`) %>%
  summarise(n = n()) %>% 
  mutate(prct = n/sum(n))
x1 %>% 
  select(idpers, `edyear$$`) %>% 
  group_by(idpers) %>% 
  mutate(mean = mean(`edyear$$`, na.rm = T)) %>% 
  select(-`edyear$$`) %>%
  distinct() %>% 
  ggplot() + 
  geom_histogram(aes(x = mean), binwidth = 1)
x1 %>% 
  select(idpers, generation) %>% 
  distinct() %>% 
  group_by(generation) %>%
  summarise(n = n()) %>% 
  mutate(prct = n/sum(n))
x1$`canton$$` <-
  factor(x1$`canton$$`, 
         levels = c(1:26), 
         labels = c("Argovia", "Appenzell Inner-Rhodes", "Appenzell\nOuter-Rhodes", "Berne", "Basle-Town", "Basle-Country", "Fribourg", "Geneva", "Glarus", "Grisons", "Jura", "Lucerne", "Neuchatel", "Nidwalden", "Obwalden", "St. Gall", "Schaffhausen", "Solothurn", "Schwyz", "Thurgovia", "Ticino", "Uri", "Vaud", "Valais", "Zug", "Zurich"))
x1 %>% 
  select(idpers, `canton$$`) %>% 
  distinct() %>% 
  group_by(idpers) %>%
  slice(1) %>% 
  group_by(`canton$$`) %>% 
  summarise(n = n()) %>% 
  mutate(prct = n / sum(n)) %>% 
  ggplot() +
  geom_bar(aes(fct_reorder(as.factor(`canton$$`), prct), prct), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5))
x1 %>% 
  select(idpers, iptotni) %>%
  group_by(idpers) %>%
  mutate(mean = mean(iptotni, na.rm = T)) %>% 
  select(-iptotni) %>%
  distinct() %>% 
  ggplot() + 
  geom_histogram(aes(mean), binwidth = 10000) +
  xlim(0,200000)

  ## slightly older sample (Boomers and Post War gen represent >50%), 
  ## women tend to be over represented (women represent 2/3), 
  ## slightly lower educated (most people are high school educated or lower)
  ## slightly lower income
  ## seems pretty well distributed across cantons of Switzerland


# Testing hypotheses
  ## Hypothesis test no.1
  ## Question_n: 18
  ## Question: Does suffering transform our sense of self-control?

h1_data <-
  tibble(
    h1a = list(extract_variables(mol_ed01_special_data, "ctrl_dn")),
    h1b = list(extract_variables(mol_ed01_special_data, "ctrl_fn")),
    h1c = list(extract_variables(mol_ed01_special_data, "ctrl_wn")),
    h1d = list(extract_variables(mol_ed01_special_data, "ctrl_hp")),
    h1e = list(extract_variables(mol_ed01_special_data, "ctrl_ot")),
    h1f = list(extract_variables(mol_ed01_special_data, "ctrl_ps")),
    h1g = list(extract_variables(mol_ed01_special_data, "ctrl_lt"))
  )

h1_alternative <- 
  c(rep("greater", 4), 
    rep("less", 3))

h1_test <- 
  perform_multiple_T_tests(7, h1_data, h1_alternative)

print_htest(h1_test, 7)

  ## Hypothesis test no.2
  ## Question_n: 19
  ## Question: Does suffering make us more resilient?

h2_data <-
  tibble(
    h2a = list(extract_variables(mol_ed01_special_data, "rsl_ovrprbls")),
    h2b = list(extract_variables(mol_ed01_special_data, "rsl_chspssblt"))
    )

h2_alternative <-
  c(rep("less", 2))

h2_test <-
  perform_multiple_T_tests(2, h2_data, h2_alternative)

print_htest(h2_test, 2)

## Hypothesis test no.3
## Question_n: 20
## Question: Does suffering profoundly change our relationship with the good things in our lives (i.e. job, family)?

h3_data <-
  tibble(
    h3a = list(extract_variables(mol_ed01_special_data, "gdthgs_j")),
    h3b = list(extract_variables(mol_ed01_special_data, "gdthgs_r")),
    h3c = list(extract_variables(mol_ed01_special_data, "gdthgs_f"))
  )

h3_alternative <-
  c(rep("less", 3))

h3_test <-
  perform_multiple_T_tests(3, h3_data, h3_alternative)

print_htest(h3_test, 3)

## Hypothesis test no.4
## Question_n: 21
## Question: Does suffering strengthen our relationship with God?

h4_data <-
  tibble(
    h4a = list(extract_variables(mol_ed01_special_data, "god_p")),
    h4b = list(extract_variables(mol_ed01_special_data, "god_t")),
    h4c = list(extract_variables(mol_ed01_special_data, "god_b")),
    h4d = list(extract_variables(mol_ed01_special_data, "god_m")),
    h4e = list(extract_variables(mol_ed01_special_data, "god_f")),
    h4f = list(extract_variables(mol_ed01_special_data, "god_g"))
  )

h4_alternative <-
  c(rep("less", 6))

h4_test <-
  perform_multiple_T_tests(6, h4_data, h4_alternative)

print_htest(h4_test, 6)

## Hypothesis test no.5
## Question_n: 22
## Question: Does suffering make us more useful to other (i.e. more compassionate)?

h5_data <-
  tibble(
    h5a = list(extract_variables(mol_ed01_special_data, "othrs_t")),
    h5b = list(extract_variables(mol_ed01_special_data, "othrs_d")),
    h5c = list(extract_variables(mol_ed01_special_data, "othrs_s")),
    h5d = list(extract_variables(mol_ed01_special_data, "othrs_vltr1")),
    h5e = list(extract_variables(mol_ed01_special_data, "othrs_vltr2"))
  )

h5_alternative <-
  c(rep("less", 2), "greater", rep("less", 2))

h5_test <-
  perform_multiple_T_tests(5, h5_data, h5_alternative)

print_htest(h5_test, 5)

## Hypothesis test no.6
## Question_n: 24
## Question: Does the impact of suffering differ on Christians as opposed to the overall population?

h6_data <-
  tibble(
    h6a = list(extract_variables(mol_ed01_special_christian_data, "othrs_t")))

h6_data <- 
  NULL

for (i in seq_along(var_names[1:23])) {
  
  h6_data <-
    tibble(
      hypothesis = list(extract_variables(mol_ed01_special_christian_data, var_names[1:23][i]))) %>% 
    bind_cols(h6_data)
  
  colnames(h6_data) <- var_names[1:23][i:1]
}

h6_alternative <-
  rev(c(h2_alternative, h1_alternative, h5_alternative, h3_alternative, h4_alternative))

h6_test <-
  perform_multiple_T_tests(23, h6_data, h6_alternative)

print_htest(h6_test, 23)


# Visualizing results for LinkedIn post
vars_to_visualise <- 
  var_names[str_starts(var_names,"ctrl")]

ave_diff <- 
  NULL

for (i in seq_along(vars_to_visualise)) {
  
  ave_diff <-
    extract_variables(mol_ed01_special_data, vars_to_visualise[i]) %>% 
    map_dfc(mean) %>% 
    gather(ends_with("before"), ends_with("after"), key = "item", value = "value") %>% 
    mutate(depression = ifelse(str_ends(item, "after"), 1, 0), item = vars_to_visualise[i]) %>% 
    bind_rows(ave_diff)
  
}


red <- "#ee3124"
blue <- "#0095da"
green <- "#009248"
yellow <- "#fdb913"

ave_diff %>% 
  ggplot() + 
  geom_line(
    aes(as_factor(depression), value, group = item, color = item), 
    linewidth = 1, show.legend = FALSE) + 
  labs(
    y = NULL, 
    x = NULL, 
    title = "Do you really want to hurt me?", 
    subtitle = "Average score across different items before and after depression",
    caption = "Note: All but one item are statistically significant at a 5% level. Items are recorded on a scale from 0\n(strongly disagree) to 10 (strongly agree). From top to bottom, the items read: 'I am doing everything\nset in my mind,''I will find a way to succeed,' 'What I want is in my hands,' 'What will happen depends\non me,' 'Others determine what I can do,' 'I feel being pushed in my life,' 'I have little influence on life\nevents.'") +
  scale_x_discrete(
    labels = c("Before\nDepression", "After\nDepression"), 
    expand = c(-0.65, 0.71)) + 
  scale_y_continuous(limits = c(2,8), breaks = seq(2,8, 1), minor_breaks = NULL) + 
  scale_color_manual(values = c(rep(blue, 3), rep(yellow, 3), blue)) + 
  geom_text(
    aes(x = 1.5, y = 2.75, label = "People feel they\nare more vulnerable"), 
    size = 3.75, color = yellow) + 
  geom_text(
    aes(x = 1.5, y = 6.25, label = "People feel they\nhave less control"), 
    size = 3.75, color = blue) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linewidth = 0.1),
    text = element_text(family = "Helvetica"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, vjust = -1, size = 7.5)
    ) 

ggsave("mol_h1_20240103_ed01.png", path = "Visuals/mol", width = 5, height = 7.5)

