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

colnames(merged_data_shp)[colnames(merged_data_shp) %in% raw_var_names] <- 
  proper_var_names

merged_data_shp <- 
  merged_data_shp %>% 
  mutate(rel_confession = 
           ifelse(`p$$r01` %in% c(1,2,3,4,9,10), 1,
                  ifelse(`p$$r01` < 0, NA, 0)),
         age_groups = 
           cut(`age$$`, 
               breaks = seq(25, 75, by = 5), 
               labels = c("25-30", "30-35", "35-40", "40-45", "45-50", "50-55",
                          "55-60", "60-65", "65-70", "70-75")),
         age = `age$$`)


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
           all_of(proper_var_names), 
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
#vis01 <-
  tab_depression %>% 
  group_by(age_groups, num_of_depressive_events) %>% 
  summarise(n = n(), mean_n = sum(!is.na(ctrl_hppndpds)), mean_ctrl_hppndpds = mean(ctrl_hppndpds, na.rm = T)) %>% 
  filter(!is.na(age_groups)) %>% 
  ggplot(aes(x = age_groups, y = mean_ctrl_hppndpds, group = num_of_depressive_events, color = num_of_depressive_events)) + 
  geom_point() +
  geom_smooth(method = "loess", se = T, aes(fill = num_of_depressive_events), show.legend = F) + 
  scale_y_continuous(limits = c(6.5,8.6), minor_breaks = NULL) +
  #scale_x_continuous(breaks = seq(2001, 2022, 3), limits = c(2001, 2022), minor_breaks = NULL) +
  scale_fill_manual(values = c(yellow, blue, red)) +
  scale_color_manual(values = c(yellow, blue, red)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.25),
        legend.position = "top") +
  guides(color = guide_legend(title = "Depressive Events:"))

model_smooth_lines <- function(data, dep_var, indep_var, span_degree = 0.75) {
  model <- loess(dep_var ~ indep_var, data = data, span = span_degree, method = "loess")
  predict(model, newdata = data)
}

pred_ctrl_hppndpds <-
  model_smooth_lines(
    tab_depression, 
    tab_depression$ctrl_hppndpds, 
    tab_depression$years_since_depressive_event
    )
pred_ctrl_lttlinflf <-
  model_smooth_lines(
    tab_depression, 
    tab_depression$ctrl_lttlinflf, 
    tab_depression$years_since_depressive_event
  )
smoothed_data <- 
  tibble(
    years_since_depressive_event = tab_depression$years_since_depressive_event, 
    ctrl_hppndpds = pred_ctrl_hppndpds,
    ctrl_lttlinflf = pred_ctrl_lttlinflf 
    )

#vis02 <- 
  tab_depression %>% 
  group_by(years_since_depressive_event, depression) %>% 
  summarise(n = n(), sample_size = sum(!is.na(ctrl_hppndpds)), mean_ctrl_hppndpds = mean(ctrl_hppndpds, na.rm = T)) %>%
  filter(!is.na(depression) & !is.na(years_since_depressive_event) & sample_size > 30) %>% 
  ggplot(aes(x = years_since_depressive_event, y = mean_ctrl_hppndpds)) + 
  geom_vline(xintercept = 0, linewidth = 0.75, color = grey) + 
  geom_point(aes(size = sample_size, alpha = sample_size), color = green) + 
  geom_line(data = smoothed_data %>%
              group_by(years_since_depressive_event) %>% 
              summarise(mean_ctrl_hppndpds = mean(ctrl_hppndpds, na.rm = T)), 
            aes(x = years_since_depressive_event, y = mean_ctrl_hppndpds), 
            color = green, linewidth = 1.5,
            arrow = arrow(type = "open", length = unit(0.5, "cm"))) + 
  #geom_smooth(aes(group = depression), method = "lm") +
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
  geom_segment(aes(x = -5, y = 6.75, xend = -6, yend = 6.75), 
               arrow = arrow(length = unit(0.25,"cm")), 
               linewidth = 0.5,
               color = grey) + 
  geom_segment(aes(x = 5, y = 6.75, xend = 6, yend = 6.75), 
               arrow = arrow(length = unit(0.25,"cm")), 
               linewidth = 0.5,
               color = grey) +
  geom_text(aes(x = -3.5, y = 6.75, label = "Yrs Before\nDepression"), 
            size = 3, 
            color = grey) +
  geom_text(aes(x = 3.5, y = 6.75, label = "Yrs After\nDepression"), 
            size = 3, 
            color = grey) +
  scale_y_continuous(limits = c(6.5, 8.6), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(-10, 10, 2), limits = c(-10, 10), minor_breaks = NULL) + 
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.25),
        legend.position = "top") +
  guides(alpha = guide_legend(title = "Sample size:"), size = guide_legend(title = "Sample size:"))


segment_position_ctrl_hppndpds_02 <-
  tibble(x = -5, y = 6.25, xend = -6, yend = y)
text_position_ctrl_hppndpds_02 <- 
  data.frame(x = -3.5, y = 6.25) 
scale_y_limits_ctrl_hppndpds <-
  c(6,9)
avg_position_ctrl_hppndpds <-
  data.frame(x = -8.5, y = -0.05)
statement_ctrl_hppndpds <-
  "What will happen depends on me"
segment_position_ctrl_hppndpds_01 <- 
  data.frame(x = 10, xend = 10, y = 8.6, yend = 8.8)
text_position_ctrl_hppndpds_01 <- 
  data.frame(x = 9, y = 8.65, x_2 = 8.75, y_2 = 8.35)

segment_position_ctrl_lttlinflf_02 <-
  tibble(x = -5, y = 3.25, xend = -6, yend = y)
text_position_ctrl_lttlinflf_02 <- 
  data.frame(x = -3.5, y = 3.25) 
scale_y_limits_ctrl_lttlinflf <-
  c(3,6)
avg_position_ctrl_lttlinflf <-
  data.frame(x = -8.5, y = 0.05)
statement_ctrl_lttlinflf <-
  "I have little influence on life events"
segment_position_ctrl_lttlinflf_01 <- 
  data.frame(x = -10, xend = -10, y = 5.6, yend = 5.8)
text_position_ctrl_lttlinflf_01 <- 
  data.frame(x = -9, y = 5.65, x_2 = -8.75, y_2 = 5.35)

panel_text_size <- 3
plot_text_size <- 12.5



f01 <- function(
    data, 
    field, 
    color,
    segment_position_01,
    text_position_01,
    avg_position,
    statement,
    segment_position_02,
    text_position_02,
    scale_y_limits,
    plot_title = NULL,
    plot_subtitle = NULL,
    plot_caption = NULL
    ) {
  #browser()
  table <-
    data %>%
    group_by(years_since_depressive_event, depression) %>%
    summarise(
      n = n(),
      sample_size = sum(!is.na({{ field }})),
      mean_ctrl = mean({{ field }}, na.rm = TRUE)
      ) %>% 
    filter(
      !is.na(depression) & !is.na(years_since_depressive_event) & sample_size > 30
      )
  
  step_1 <-
    table %>% 
    ggplot(aes(x = years_since_depressive_event, y = mean_ctrl))
 
  depression_score <- 
    table %>% 
    ungroup() %>% 
    filter(years_since_depressive_event == 0) %>% 
    select(mean_ctrl) %>% unlist()
   
  step_2 <- 
    step_1 +
    geom_segment(
      aes(x = -1.5, xend = -0.5, y = depression_score, yend = depression_score),
      arrow = arrow(length = unit(0.25,"cm")),
      linewidth = 0.25,
      color = grey) +
    geom_text(
      aes(x = 0 - 3.25, y = depression_score, label = "1st Recorded\nDepression"),
      size = panel_text_size, 
      color = grey)
    
  step_3 <-
    step_2 +
    geom_vline(xintercept = 0, linewidth = 0.75, color = grey) + 
    geom_point(aes(size = sample_size, alpha = sample_size), color = color) + 
    geom_line(data = smoothed_data %>%
                group_by(years_since_depressive_event) %>% 
                summarise(mean_smooth = mean({{ field }}, na.rm = T)), 
              aes(x = years_since_depressive_event, y = mean_smooth), 
              color = color, linewidth = 1.5,
              arrow = arrow(type = "open", length = unit(0.5, "cm")))
  
  mean_before <-
    data %>%
    filter(years_since_depressive_event < 0 & !is.na({{ field }})) %>% 
    select({{ field }}) %>% 
    summarise(mean = mean({{ field }})) %>% 
    unlist()
  mean_after <-
    data %>% 
    filter(years_since_depressive_event > 0 & !is.na({{ field }})) %>% 
    select({{ field }}) %>% 
    summarise(mean = mean({{ field }})) %>% 
    unlist()
  
  step_4 <-
    step_3 +
    geom_segment(aes(y = mean_before, 
                     yend =  mean_before, 
                     x = -10, xend = 0), 
                 linewidth = 0.2, linetype = 2, color = grey) + 
    geom_segment(aes(y = mean_after, 
                     yend = mean_after, 
                     x = 0, xend = 10), 
                 linewidth = 0.2, linetype = 2, color = grey) +
    geom_text(data = avg_position,
              aes(x = x, y = mean_before + y, 
                  label = paste0("mean: ", round(mean_before,2))),
              size = panel_text_size, 
              color = grey) +
    geom_text(data = avg_position,
              aes(x = abs(x), y = mean_after - abs(y), 
                  label = paste0("mean: ", round(mean_after,2))),
              size = panel_text_size, 
              color = grey)
  step_5 <-
    step_4 +
    geom_label(aes(x = 0, y = scale_y_limits[2], label = statement),
               fill = color,
               color = "white") 
  
  step_6 <-
    step_5 +
    geom_segment(data = segment_position_01, 
                 aes(x = x, xend = xend, y = y, yend = yend),
                 arrow = arrow(length = unit(0.25,"cm")), 
                 linewidth = 0.5,
                 color = grey) +
    geom_segment(data = segment_position_01, 
                 aes(x = x, xend = xend, y = y - 0.2, yend = yend - 0.6),
                 arrow = arrow(length = unit(0.25,"cm")), 
                 linewidth = 0.5,
                 color = grey) +
    geom_text(data = text_position_01,
              aes(x = x, y = y, label = "Agree"), 
              size = panel_text_size, 
              color = grey) +
    geom_text(data = text_position_01,
              aes(x = x_2, y = y_2, label = "Disagree"), 
              size = panel_text_size, 
              color = grey) 
    
  step_7 <-
    step_6 + 
    geom_segment(data = segment_position_02,
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 arrow = arrow(length = unit(0.25,"cm")), 
                 linewidth = 0.5,
                 color = grey) + 
    geom_segment(data = abs(segment_position_02),
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 arrow = arrow(length = unit(0.25,"cm")), 
                 linewidth = 0.5,
                 color = grey) +
    geom_text(data = text_position_02,
              aes(x = x, y = y, label = "Yrs Before\nDepression"), 
              size = panel_text_size, 
              color = grey) +
    geom_text(data = abs(text_position_02),
              aes(x = x, y = y, label = "Yrs After\nDepression"), 
              size = panel_text_size, 
              color = grey) +
    scale_y_continuous(limits = scale_y_limits, minor_breaks = NULL) +
    scale_x_continuous(breaks = seq(-10, 10, 2), limits = c(-10, 10), minor_breaks = NULL) + 
    labs(x = NULL, y = NULL,
         title = plot_title,
         subtitle = plot_subtitle,
         caption = plot_caption) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), 
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linewidth = 0.25),
          legend.position = "top",
          text = element_text(size = plot_text_size)) +
    guides(alpha = guide_legend(title = "Sample size:"), size = guide_legend(title = "Sample size:"))
  
  return(step_7)
}

plot_ctrl_lttlinflf_years <-
  f01(
    data = tab_depression, 
    field = ctrl_lttlinflf, 
    color = yellow, 
    segment_position_01 = segment_position_ctrl_lttlinflf_01,
    text_position_01 = text_position_ctrl_lttlinflf_01,
    scale_y_limits = scale_y_limits_ctrl_lttlinflf, 
    segment_position_02 = segment_position_ctrl_lttlinflf_02, 
    avg_position = avg_position_ctrl_lttlinflf, 
    text_position_02 = text_position_ctrl_lttlinflf_02, 
    statement = statement_ctrl_lttlinflf, 
    plot_title = "How Depression Shapes Our Sense of Control", 
    plot_subtitle = "Average agreement with the statements: \"I have little influence on life events\" (left) and \"What will happen depends on me\" (right)\non a scale from 0 (disagree) to 10 (agree), before and after the first recorded depression.",
    plot_caption = "Source: Swiss Household Panel (SHP), author's calculation\nAuthor: Bruno Alves de Carvalho (balvesdecarvalho1906@gmail.com)"
    ) 
  
plot_ctrl_hppndpds_years <-
  f01(
    data = tab_depression, 
    field = ctrl_hppndpds, 
    color = green, 
    segment_position_01 = segment_position_ctrl_hppndpds_01,
    text_position_01 = text_position_ctrl_hppndpds_01,
    scale_y_limits = scale_y_limits_ctrl_hppndpds, 
    segment_position_02 = segment_position_ctrl_hppndpds_02, 
    avg_position = avg_position_ctrl_hppndpds, 
    text_position_02 = text_position_ctrl_hppndpds_02, 
    statement = statement_ctrl_hppndpds, 
    plot_title = "", 
    plot_subtitle = "\n", 
    plot_caption = "\n"
    )

combined_plot_ctrl_years <- gridExtra::grid.arrange(plot_ctrl_lttlinflf_years, plot_ctrl_hppndpds_years, ncol = 2)

ggsave("Ep01_PlotCtrlYears_20240218_ve01.png", combined_plot_ctrl_years, path = "/Users/brunoalvesdecarvalho/Desktop/R Projects/fact-or-legend-ep01-suffering", width = 10.5, height = 6.5)
ggsave("Ep01_PlotCtrlYears_20240218_ve01.jpeg", combined_plot_ctrl_years, path = "/Users/brunoalvesdecarvalho/Desktop/R Projects/fact-or-legend-ep01-suffering" , width = 10.5, height = 6.5)







