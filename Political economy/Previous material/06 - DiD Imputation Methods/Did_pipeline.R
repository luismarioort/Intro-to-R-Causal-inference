# Author: JOSE

#######
rm(list = ls())
options(rgl.useNULL = TRUE)

## llamando a las librerias que (creemos que) vamos a usar
options("install.lock"=FALSE)

if (!require ("pacman")) install.packages ("pacman")

if ("paneltools" %in% rownames(installed.packages()) == FALSE) {
  devtools:: install_github("xuyiqing/paneltools")
}

pacman::p_load(dplyr
               , haven
               , ggplot2
               , ggpubr
               , ggthemes  ## si quieren mas themes
               , readstata13
               , readxl
               , sf
               , tidyverse
               , tidyr
               , units
               , viridis ## paleta de colores Viridis
               , wesanderson## p/usar paleta de colores de Wes Anderson
               , stringr
               , RColorBrewer
               , patchwork
               , Rmisc
               , lfe
               , stargazer
               , AER
               , haven
               , skimr
               , modelsummary
               , terra
               , did
               , didimputation
               , DIDmultiplegt
               , did2s
               , GRShiny
               , broom
               , paneltools
               , ggfixest
               , purrr
               , rgl
               , DIDmultiplegtDYN
               , kableExtra
               , DescTools
               , devtools
               , contdid)

setwd("~/Documents/All_files/NASA")
options(scipen = 999)

create_results_plot <- function(data, x, y, xmin, xmax, method_label) {
  ggplot(
    data = data, 
    aes(x = !!sym(x), y = factor(!!sym(y)))
  ) + 
    geom_point(size = 3, color = "black") + 
    geom_errorbar(aes(xmin = !!sym(xmin), xmax = !!sym(xmax)), width = 0.2, color = "black") + 
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) + 
    theme_minimal(base_size = 16, base_family = "serif") +  # Minimal theme for a clean look
    xlab("Estimated Coefficient (95% CI)") + 
    ylab(method_label) + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),  # Remove major gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.line = element_line(color = "black")  # Add axis lines for clarity
    )
}

create_methodology_plot <- function(data, title, model_label) {
  # Calculate overall y-axis limits
  y_min <- min(data$LB_CI, na.rm = TRUE) - 0.00001
  y_max <- max(data$UB_CI, na.rm = TRUE) + 0.00001
  
  ggplot(data, aes(x = as.factor(time), y = Estimate)) +
    geom_point(size = 3, color = "black") +
    geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal(base_size = 16, base_family = "serif") +  # Minimal theme for a cleaner look
    coord_cartesian(ylim = c(y_min, y_max)) +  # Consistent y-axis limits
    xlab("Time to treatment") +
    ylab("Marginal Effect Coefficient") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 12, hjust = 0.5, face = "italic", margin = margin(t = 5)),
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      panel.grid.major = element_blank(),  # Remove major gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      axis.line = element_line(color = "black"),  # Add axis lines for clarity
      plot.margin = margin(10, 10, 10, 10)  # Add spacing between panels
    )
}

##############################################################
# COMPREHENSIVE MODELS PIPELINE FOR MULTIPLE TRANSFORMATIONS AND STAGGERED ADOPTION
# INCLUDING TREATMENT REVERSION AND INTENSIVE MARGIN ANALYSES
##############################################################

#Transformations:

# 1. INVERSE HYPERBOLIC SINE (IHS) TRANSFORMATION
# 2. LOGARITHMIC (+1) TRANSFORMATION
# 3. BINARY TRANSFORMATION (INDICATOR OF POSITIVE VALUES)
# 4. CHEN AND ROTH (2024) TRANSFORMATION

# Methodologies structure:

# - TWFE (Two-Way Fixed Effects)
# - Callaway & Sant'Anna (2021) - absorbent
# - Callaway & Sant'Anna (2025) - Intensive Margin
# - Borusyak et al. (2024) absorbent
# - de Chaisemartin & D'Haultfoeuille (2020) - absorbent
# - de Chaisemartin & D'Haultfoeuille (2020) - Treatment Reversion
# - de Chaisemartin & D'Haultfoeuille (2020) - Intensive Margin

#Outcomes structure:

#IHS_Total_articles
#Log1p_Total_articles

#Binary_Total_articles - cuando tienes muchos ceros
#LogLinear_Total_articles - cuando tienes muchos ceros

names(df.twfe_news)

#database structure:

df.twfe_selected <- df.twfe |> 
  dplyr::select(Year,INEGI_CODE,Total_presence,Dich_Presence,Cohort)

df.twfe_news <- df.twfe_news |> 
  dplyr::left_join(df.twfe_selected,
                   by = c("Year","INEGI_CODE"))

##########################################
# 1. INVERSE HYPERBOLIC SINE (IHS) TRANSFORMATION
##########################################

# TWFE model
plot_twfe_IHS <- feols(IHS_Total_articles ~ i(Time_to_Treatment, Treatment_group, -1) | INEGI_CODE + Year,  
                       data = df.twfe_news, cluster = "INEGI_CODE")

iplot(plot_twfe_IHS)

twfe_IHS_output <- tibble::tibble(
  rowname = names(plot_twfe_IHS$coefficients),
  Estimate = plot_twfe_IHS$coefficients,
  SE = plot_twfe_IHS$se
) %>% 
  dplyr::mutate(
    rowname = gsub("Time_to_Treatment::", "", rowname),
    rowname = gsub(":Treatment_group", "", rowname),
    time = as.numeric(rowname),
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "TWFE"
  ) %>%
  dplyr::select(-rowname, -SE) %>%
  dplyr::filter(time >= -5 & time <= 5)

twfe_IHS_output <- dplyr::bind_rows(
  twfe_IHS_output,
  tibble::tibble(time = -1, Estimate = 0, LB_CI = 0, UB_CI = 0, Model = "TWFE")
)

# Callaway and Sant'Anna model
cs_IHS <- att_gt(
  yname = "IHS_Total_articles",
  gname = "firstdto",
  idname = "INEGI_CODE",
  tname = "Year",
  xformla = ~1,
  control_group = "notyettreated",
  data = df.twfe_news,
  base_period = "universal"
)

cs_IHS_agg <- aggte(cs_IHS, type = "dynamic", bstrap = FALSE, cband = FALSE, na.rm = TRUE)

cs_IHS_output <- tibble::tibble(
  time = cs_IHS_agg$egt,
  Estimate = cs_IHS_agg$att.egt,
  SE = cs_IHS_agg$se.egt
) %>%
  dplyr::filter(time >= -5 & time <= 5) %>%
  dplyr::mutate(
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "CS"
  ) %>%
  dplyr::select(-SE)

# Borusyak estimation - Improved approach with separate pre/post estimations
# Post-treatment effects
borus_post_IHS <- did_imputation(
  data = df.twfe_news, 
  yname = "IHS_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE",
  horizon = c(0:5)  # Only post-treatment effects
)

# Pre-treatment (placebo) effects
borus_pre_IHS <- did_imputation(
  data = df.twfe_news, 
  yname = "IHS_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE",
  pretrends = c(-5:-1)  # Only pre-treatment effects
)

# Format results for plotting
borus_post_IHS_output <- borus_post_IHS %>%
  dplyr::mutate(
    time = as.numeric(term),
    Model = "Borusyak Post",
    Estimate = estimate,
    LB_CI = conf.low,
    UB_CI = conf.high
  ) %>%
  dplyr::select(time, Estimate, LB_CI, UB_CI, Model) %>% 
  dplyr::filter(time <= 5)

borus_pre_IHS_output <- borus_pre_IHS %>%
  dplyr::mutate(
    time = as.numeric(term),
    Model = "Borusyak Pre",
    Estimate = estimate,
    LB_CI = conf.low,
    UB_CI = conf.high
  ) %>%
  dplyr::select(time, Estimate, LB_CI, UB_CI, Model) %>% 
  dplyr::filter(time >= -5)

# de Chaisemartin and D'Haultfoeuille model - absorbent
dcdh_IHS <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "IHS_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "DTO_dummy_a", #absorbing treatment
  placebo = 50,
  effects = 50,
  controls = c("a","b","c"),
  graph_off = TRUE
)

dcdh_IHS_output <- as.data.frame(dcdh_IHS[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(Model = "dCDH") %>% 
  dplyr::mutate(time = time - 1) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# de Chaisemartin and D'Haultfoeuille - Treatment Reversion (Non-absorbing)
# Assuming Dich_Presence variable exists in df.twfe_news for non-absorbing treatment
dcdh_reversion_IHS <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "IHS_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "Dich_Presence",  # Non-absorbing treatment
  effects = 50,
  placebo = 50,
  graph_off = TRUE,
  cluster = "INEGI_CODE")

?did_multiplegt_dyn

dcdh_reversion_IHS_output <- as.data.frame(dcdh_reversion_IHS[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(
    Model = "dCDH Reversion",
    time = time - 1
  ) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# de Chaisemartin and D'Haultfoeuille - Intensive Margin (Continuous treatment)
# Assuming Total_presence variable exists in df.twfe_news for continuous treatment
dcdh_intensive_IHS <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "IHS_Total_articles",
  group = "INEGI_CODE",
  time = "Year",#period = year-month
  treatment = "Total_presence",  # Continuous treatment
  effects = 50,
  placebo = 50,
  graph_off = TRUE,
  cluster = "INEGI_CODE"
)

dcdh_intensive_IHS_output <- as.data.frame(dcdh_intensive_IHS[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(
    Model = "dCDH Intensive",
    time = time - 1
  ) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# Set min and max values for consistent y-axis in plots
y_min_IHS <- min(c(min(borus_pre_IHS_output$LB_CI), min(borus_post_IHS_output$LB_CI)))
y_max_IHS <- max(c(max(borus_pre_IHS_output$UB_CI), max(borus_post_IHS_output$UB_CI)))

# Create the placebo plot for IHS
placebo_plot_IHS <- ggplot(borus_pre_IHS_output, aes(x = time, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "blue") +
  labs(x = "Time to Treatment",
       y = "Marginal Effect Coefficient") +
  scale_y_continuous(limits = c(y_min_IHS, y_max_IHS)) +
  theme_minimal(base_size = 16, base_family = "serif") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
  )

# Create the treatment effects plot for IHS
treatment_plot_IHS <- ggplot(borus_post_IHS_output, aes(x = time, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "blue") +
  labs(x = "Time Since Treatment",
       y = "Marginal Effect Coefficient") +
  scale_y_continuous(limits = c(y_min_IHS, y_max_IHS), position = "right") +
  theme_minimal(base_size = 16, base_family = "serif") +
  theme(
    axis.title.y.right = element_text(size = 14, face = "bold"),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
  )

# Combine the plots
plot_borus_IHS <- placebo_plot_IHS + treatment_plot_IHS +
  patchwork::plot_layout(widths = c(1, 1)) +
  patchwork::plot_annotation(
    theme = theme(
      plot.caption = element_text(size = 12, face = "italic", hjust = 0.5, margin = margin(t = 10))
    )
  )

# Event study plots for each model (individually)
plot_twfe_IHS <- create_methodology_plot(
  twfe_IHS_output, 
  ""
)

plot_cs_IHS <- create_methodology_plot(
  cs_IHS_output, 
  "")

plot_dcdh_IHS <- create_methodology_plot(
  dcdh_IHS_output, 
  "")

# Additional event study plots for reversion and intensive margin

plot_dcdh_reversion_IHS <- create_methodology_plot(
  dcdh_reversion_IHS_output,
  "")

plot_dcdh_intensive_IHS <- create_methodology_plot(
  dcdh_intensive_IHS_output,
  "")


# Save each event study plot individually
ggsave("IHS_twfe_event_study.png", plot_twfe_IHS, width = 8, height = 6, dpi = 300)
ggsave("IHS_cs_event_study.png", plot_cs_IHS, width = 8, height = 6, dpi = 300)
ggsave("IHS_borus_event_study.png", plot_borus_IHS, width = 8, height = 6, dpi = 300)
ggsave("IHS_dcdh_event_study.png", plot_dcdh_IHS, width = 8, height = 6, dpi = 300)
ggsave("IHS_dcdh_reversion_event_study.png", plot_dcdh_reversion_IHS, width = 8, height = 6, dpi = 300)
ggsave("IHS_dcdh_intensive_event_study.png", plot_dcdh_intensive_IHS, width = 8, height = 6, dpi = 300)


# Calculate ATT estimates for each model
twfe_att_IHS <- felm(IHS_Total_articles ~ DTO_dummy_a | INEGI_CODE + Year | 0 | INEGI_CODE,
                     data = df.twfe_news)

coef_twfe_IHS <- c(twfe_att_IHS$coefficients)
se_twfe_IHS <- twfe_att_IHS$cse

df_twfe_att_IHS <- tibble::tibble(
  type = "TWFE", 
  coef = coef_twfe_IHS, 
  se = se_twfe_IHS,
  conf.low = coef_twfe_IHS - 1.96 * se_twfe_IHS,
  conf.high = coef_twfe_IHS + 1.96 * se_twfe_IHS
)

borus_att_IHS <- did_imputation(
  data = df.twfe_news, 
  yname = "IHS_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE"
) %>%
  dplyr::mutate(type = "Borusyak et al.") %>% 
  dplyr::select(
    type, 
    coef = estimate, 
    se = std.error, 
    conf.low, 
    conf.high
  )

agg_att_cs_IHS <- aggte(cs_IHS, type = "simple", na.rm = TRUE)

df_cs_att_IHS <- tibble::tibble(
  type = "Callaway & Sant'Anna", 
  coef = agg_att_cs_IHS$overall.att,
  se = agg_att_cs_IHS$overall.se, 
  conf.low = agg_att_cs_IHS$overall.att - 1.96 * agg_att_cs_IHS$overall.se,
  conf.high = agg_att_cs_IHS$overall.att + 1.96 * agg_att_cs_IHS$overall.se
)

coef_dcdh_IHS <- as.data.frame(dcdh_IHS$results$ATE)

df_dcdh_att_IHS <- coef_dcdh_IHS %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "de Chaisemartin & D'Haultfoeuille") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

coef_dcdh_reversion_IHS <- as.data.frame(dcdh_reversion_IHS$results$ATE)

df_dcdh_reversion_att_IHS <- coef_dcdh_reversion_IHS %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "dCDH (Reversion)") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

coef_dcdh_intensive_IHS <- as.data.frame(dcdh_intensive_IHS$results$ATE)

df_dcdh_intensive_att_IHS <- coef_dcdh_intensive_IHS %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "dCDH (Intensive)") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

# Combine ATT results and create plot
att_results_IHS <- dplyr::bind_rows(
  df_twfe_att_IHS,
  borus_att_IHS,
  df_cs_att_IHS,
  df_dcdh_att_IHS,
  df_dcdh_reversion_att_IHS,
  df_dcdh_intensive_att_IHS
)


att_results_IHS <- att_results_IHS %>%
  dplyr::filter(!is.na(coef))

att_plot_IHS <- create_results_plot(
  data = att_results_IHS, 
  x = "coef", 
  y = "type", 
  xmin = "conf.low", 
  xmax = "conf.high", 
  method_label = "Method"
)

# Save ATT plot
ggsave("IHS_att.png", att_plot_IHS, width = 8, height = 6, dpi = 300)

##########################################
# 2. LOGARITHMIC (+1) TRANSFORMATION
##########################################

# TWFE model
plot_twfe_Log1p <- feols(Log1p_Total_articles ~ i(Time_to_Treatment, Treatment_group, -1) | INEGI_CODE + Year,  
                         data = df.twfe_news, cluster = "INEGI_CODE")

twfe_Log1p_output <- tibble::tibble(
  rowname = names(plot_twfe_Log1p$coefficients),
  Estimate = plot_twfe_Log1p$coefficients,
  SE = plot_twfe_Log1p$se
) %>% 
  dplyr::mutate(
    rowname = gsub("Time_to_Treatment::", "", rowname),
    rowname = gsub(":Treatment_group", "", rowname),
    time = as.numeric(rowname),
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "TWFE"
  ) %>%
  dplyr::select(-rowname, -SE) %>%
  dplyr::filter(time >= -5 & time <= 5)

twfe_Log1p_output <- dplyr::bind_rows(
  twfe_Log1p_output,
  tibble::tibble(time = -1, Estimate = 0, LB_CI = 0, UB_CI = 0, Model = "TWFE")
)

# Callaway and Sant'Anna model
cs_Log1p <- att_gt(
  yname = "Log1p_Total_articles",
  gname = "firstdto",
  idname = "INEGI_CODE",
  tname = "Year",
  xformla = ~1,
  control_group = "notyettreated",
  data = df.twfe_news,
   base_period = "universal"
)

cs_Log1p_agg <- aggte(cs_Log1p, type = "dynamic", bstrap = FALSE, cband = FALSE, na.rm = TRUE)

cs_Log1p_output <- tibble::tibble(
  time = cs_Log1p_agg$egt,
  Estimate = cs_Log1p_agg$att.egt,
  SE = cs_Log1p_agg$se.egt
) %>%
  dplyr::filter(time >= -5 & time <= 5) %>%
  dplyr::mutate(
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "CS"
  ) %>%
  dplyr::select(-SE)

# Borusyak estimation - Improved approach with separate pre/post estimations
# Post-treatment effects
borus_post_Log1p <- did_imputation(
  data = df.twfe_news, 
  yname = "Log1p_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE",
  horizon = c(0:5)  # Only post-treatment effects
)

# Pre-treatment (placebo) effects
borus_pre_Log1p <- did_imputation(
  data = df.twfe_news, 
  yname = "Log1p_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE",
  pretrends = c(-5:-1)  # Only pre-treatment effects
)

# Format results for plotting
borus_post_Log1p_output <- borus_post_Log1p %>%
  dplyr::mutate(
    time = as.numeric(term),
    Model = "Borusyak Post",
    Estimate = estimate,
    LB_CI = conf.low,
    UB_CI = conf.high
  ) %>%
  dplyr::select(time, Estimate, LB_CI, UB_CI, Model) %>% 
  dplyr::filter(time <= 5)

borus_pre_Log1p_output <- borus_pre_Log1p %>%
  dplyr::mutate(
    time = as.numeric(term),
    Model = "Borusyak Pre",
    Estimate = estimate,
    LB_CI = conf.low,
    UB_CI = conf.high
  ) %>%
  dplyr::select(time, Estimate, LB_CI, UB_CI, Model) %>% 
  dplyr::filter(time >= -5)

# de Chaisemartin and D'Haultfoeuille model - Standard
dcdh_Log1p <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "Log1p_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "DTO_dummy_a",
  placebo = 50,
  effects = 50,
  graph_off = TRUE
)

dcdh_Log1p_output <- as.data.frame(dcdh_Log1p[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(Model = "dCDH") %>% 
  dplyr::mutate(time = time - 1) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# de Chaisemartin and D'Haultfoeuille - Treatment Reversion
dcdh_reversion_Log1p <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "Log1p_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "Dich_Presence",
  effects = 50,
  placebo = 50,
  graph_off = TRUE,
  cluster = "INEGI_CODE"
)

dcdh_reversion_Log1p_output <- as.data.frame(dcdh_reversion_Log1p[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(
    Model = "dCDH Reversion",
    time = time - 1
  ) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# de Chaisemartin and D'Haultfoeuille - Intensive Margin
dcdh_intensive_Log1p <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "Log1p_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "Total_presence",
  effects = 50,
  placebo = 50,
  graph_off = TRUE,
  cluster = "INEGI_CODE"
)

dcdh_intensive_Log1p_output <- as.data.frame(dcdh_intensive_Log1p[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(
    Model = "dCDH Intensive",
    time = time - 1
  ) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# Set min and max values for consistent y-axis in plots
y_min_Log1p <- min(c(min(borus_pre_Log1p_output$LB_CI), min(borus_post_Log1p_output$LB_CI)))
y_max_Log1p <- max(c(max(borus_pre_Log1p_output$UB_CI), max(borus_post_Log1p_output$UB_CI)))

# Create the placebo plot for Log1p
placebo_plot_Log1p <- ggplot(borus_pre_Log1p_output, aes(x = time, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "blue") +
  labs(x = "Time to Treatment",
       y = "Marginal Effect Coefficient") +
  scale_y_continuous(limits = c(y_min_Log1p, y_max_Log1p)) +
  theme_minimal(base_size = 16, base_family = "serif") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# Create the treatment effects plot for Log1p
treatment_plot_Log1p <- ggplot(borus_post_Log1p_output, aes(x = time, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "blue") +
  labs(x = "Time Since Treatment",
       y = "Marginal Effect Coefficient") +
  scale_y_continuous(limits = c(y_min_Log1p, y_max_Log1p), position = "right") +
  theme_minimal(base_size = 16, base_family = "serif") +
  theme(
    axis.title.y.right = element_text(size = 14, face = "bold"),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# Combine the plots
plot_borus_Log1p <- placebo_plot_Log1p + treatment_plot_Log1p +
  patchwork::plot_layout(widths = c(1, 1)) +
  patchwork::plot_annotation(
    theme = theme(
      plot.caption = element_text(size = 12, face = "italic", hjust = 0.5, margin = margin(t = 10))
    )
  )

# Event study plots for each model (individually)
plot_twfe_Log1p <- create_methodology_plot(
  twfe_Log1p_output, 
  ""
)

plot_cs_Log1p <- create_methodology_plot(
  cs_Log1p_output, 
  ""
)

plot_dcdh_Log1p <- create_methodology_plot(
  dcdh_Log1p_output, 
  ""
)

plot_dcdh_reversion_Log1p <- create_methodology_plot(
  dcdh_reversion_Log1p_output,
  ""
)

plot_dcdh_intensive_Log1p <- create_methodology_plot(
  dcdh_intensive_Log1p_output,
  ""
)

# Save each event study plot individually
ggsave("Log1p_twfe_event_study.png", plot_twfe_Log1p, width = 8, height = 6, dpi = 300)
ggsave("Log1p_cs_event_study.png", plot_cs_Log1p, width = 8, height = 6, dpi = 300)
ggsave("Log1p_borus_event_study.png", plot_borus_Log1p, width = 8, height = 6, dpi = 300)
ggsave("Log1p_dcdh_event_study.png", plot_dcdh_Log1p, width = 8, height = 6, dpi = 300)
ggsave("Log1p_dcdh_reversion_event_study.png", plot_dcdh_reversion_Log1p, width = 8, height = 6, dpi = 300)
ggsave("Log1p_dcdh_intensive_event_study.png", plot_dcdh_intensive_Log1p, width = 8, height = 6, dpi = 300)

# Calculate ATT estimates for each model
twfe_att_Log1p <- felm(Log1p_Total_articles ~ DTO_dummy_a | INEGI_CODE + Year | 0 | INEGI_CODE,
                       data = df.twfe_news)

coef_twfe_Log1p <- c(twfe_att_Log1p$coefficients)
se_twfe_Log1p <- twfe_att_Log1p$cse

df_twfe_att_Log1p <- tibble::tibble(
  type = "TWFE", 
  coef = coef_twfe_Log1p, 
  se = se_twfe_Log1p,
  conf.low = coef_twfe_Log1p - 1.96 * se_twfe_Log1p,
  conf.high = coef_twfe_Log1p + 1.96 * se_twfe_Log1p
)

borus_att_Log1p <- did_imputation(
  data = df.twfe_news, 
  yname = "Log1p_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE"
) %>%
  dplyr::mutate(type = "Borusyak et al.") %>% 
  dplyr::select(
    type, 
    coef = estimate, 
    se = std.error, 
    conf.low, 
    conf.high
  )

agg_att_cs_Log1p <- aggte(cs_Log1p, type = "simple", na.rm = TRUE)

df_cs_att_Log1p <- tibble::tibble(
  type = "Callaway & Sant'Anna", 
  coef = agg_att_cs_Log1p$overall.att,
  se = agg_att_cs_Log1p$overall.se, 
  conf.low = agg_att_cs_Log1p$overall.att - 1.96 * agg_att_cs_Log1p$overall.se,
  conf.high = agg_att_cs_Log1p$overall.att + 1.96 * agg_att_cs_Log1p$overall.se
)

coef_dcdh_Log1p <- as.data.frame(dcdh_Log1p$results$ATE)

df_dcdh_att_Log1p <- coef_dcdh_Log1p %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "de Chaisemartin & D'Haultfoeuille") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

coef_dcdh_reversion_Log1p <- as.data.frame(dcdh_reversion_Log1p$results$ATE)

df_dcdh_reversion_att_Log1p <- coef_dcdh_reversion_Log1p %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "dCDH (Reversion)") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

coef_dcdh_intensive_Log1p <- as.data.frame(dcdh_intensive_Log1p$results$ATE)

df_dcdh_intensive_att_Log1p <- coef_dcdh_intensive_Log1p %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "dCDH (Intensive)") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

# Combine ATT results and create plot
att_results_Log1p <- dplyr::bind_rows(
  df_twfe_att_Log1p,
  borus_att_Log1p,
  df_cs_att_Log1p,
  df_dcdh_att_Log1p,
  df_dcdh_reversion_att_Log1p,
  df_dcdh_intensive_att_Log1p
)

att_results_Log1p <- att_results_Log1p %>%
  dplyr::filter(!is.na(coef))

att_plot_Log1p <- create_results_plot(
  data = att_results_Log1p, 
  x = "coef", 
  y = "type", 
  xmin = "conf.low", 
  xmax = "conf.high", 
  method_label = "Method"
)

# Save ATT plot
ggsave("Log1p_att.png", att_plot_Log1p, width = 8, height = 6, dpi = 300)

##########################################
# 3. BINARY TRANSFORMATION (INDICATOR OF POSITIVE VALUES)
##########################################

# TWFE model
plot_twfe_Binary <- feols(Binary_Total_articles ~ i(Time_to_Treatment, Treatment_group, -1) | INEGI_CODE + Year,  
                          data = df.twfe_news, cluster = "INEGI_CODE")

twfe_Binary_output <- tibble::tibble(
  rowname = names(plot_twfe_Binary$coefficients),
  Estimate = plot_twfe_Binary$coefficients,
  SE = plot_twfe_Binary$se
) %>% 
  dplyr::mutate(
    rowname = gsub("Time_to_Treatment::", "", rowname),
    rowname = gsub(":Treatment_group", "", rowname),
    time = as.numeric(rowname),
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "TWFE"
  ) %>%
  dplyr::select(-rowname, -SE) %>%
  dplyr::filter(time >= -5 & time <= 5)

twfe_Binary_output <- dplyr::bind_rows(
  twfe_Binary_output,
  tibble::tibble(time = -1, Estimate = 0, LB_CI = 0, UB_CI = 0, Model = "TWFE")
)

# Callaway and Sant'Anna model
cs_Binary <- att_gt(
  yname = "Binary_Total_articles",
  gname = "firstdto",
  idname = "INEGI_CODE",
  tname = "Year",
  xformla = ~1,
  control_group = "notyettreated",
  data = df.twfe_news,
   base_period = "universal"
)

cs_Binary_agg <- aggte(cs_Binary, type = "dynamic", bstrap = FALSE, cband = FALSE, na.rm = TRUE)

cs_Binary_output <- tibble::tibble(
  time = cs_Binary_agg$egt,
  Estimate = cs_Binary_agg$att.egt,
  SE = cs_Binary_agg$se.egt
) %>%
  dplyr::filter(time >= -5 & time <= 5) %>%
  dplyr::mutate(
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "CS"
  ) %>%
  dplyr::select(-SE)

# Borusyak estimation - Improved approach with separate pre/post estimations
# Post-treatment effects
borus_post_Binary <- did_imputation(
  data = df.twfe_news, 
  yname = "Binary_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE",
  horizon = c(0:5)  # Only post-treatment effects
)

# Pre-treatment (placebo) effects
borus_pre_Binary <- did_imputation(
  data = df.twfe_news, 
  yname = "Binary_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE",
  pretrends = c(-5:-1)  # Only pre-treatment effects
)

# Format results for plotting
borus_post_Binary_output <- borus_post_Binary %>%
  dplyr::mutate(
    time = as.numeric(term),
    Model = "Borusyak Post",
    Estimate = estimate,
    LB_CI = conf.low,
    UB_CI = conf.high
  ) %>%
  dplyr::select(time, Estimate, LB_CI, UB_CI, Model) %>% 
  dplyr::filter(time <= 5)

borus_pre_Binary_output <- borus_pre_Binary %>%
  dplyr::mutate(
    time = as.numeric(term),
    Model = "Borusyak Pre",
    Estimate = estimate,
    LB_CI = conf.low,
    UB_CI = conf.high
  ) %>%
  dplyr::select(time, Estimate, LB_CI, UB_CI, Model) %>% 
  dplyr::filter(time >= -5)

# de Chaisemartin and D'Haultfoeuille model - Standard
dcdh_Binary <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "Binary_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "DTO_dummy_a",
  placebo = 50,
  effects = 50,
  graph_off = TRUE
)

dcdh_Binary_output <- as.data.frame(dcdh_Binary[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(Model = "dCDH") %>% 
  dplyr::mutate(time = time - 1) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# de Chaisemartin and D'Haultfoeuille - Treatment Reversion
dcdh_reversion_Binary <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "Binary_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "Dich_Presence",
  effects = 50,
  placebo = 50,
  graph_off = TRUE,
  cluster = "INEGI_CODE"
)

dcdh_reversion_Binary_output <- as.data.frame(dcdh_reversion_Binary[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(
    Model = "dCDH Reversion",
    time = time - 1
  ) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# de Chaisemartin and D'Haultfoeuille - Intensive Margin
dcdh_intensive_Binary <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "Binary_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "Total_presence",
  effects = 50,
  placebo = 50,
  graph_off = TRUE,
  cluster = "INEGI_CODE"
)

dcdh_intensive_Binary_output <- as.data.frame(dcdh_intensive_Binary[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(
    Model = "dCDH Intensive",
    time = time - 1
  ) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# Set min and max values for consistent y-axis in plots
y_min_Binary <- min(c(min(borus_pre_Binary_output$LB_CI), min(borus_post_Binary_output$LB_CI)))
y_max_Binary <- max(c(max(borus_pre_Binary_output$UB_CI), max(borus_post_Binary_output$UB_CI)))

# Create the placebo plot for Binary
placebo_plot_Binary <- ggplot(borus_pre_Binary_output, aes(x = time, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "blue") +
  labs(x = "Time to Treatment",
       y = "Marginal Effect Coefficient") +
  scale_y_continuous(limits = c(y_min_Binary, y_max_Binary)) +
  theme_minimal(base_size = 16, base_family = "serif") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# Create the treatment effects plot for Binary
treatment_plot_Binary <- ggplot(borus_post_Binary_output, aes(x = time, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "blue") +
  labs(x = "Time Since Treatment",
       y = "Marginal Effect Coefficient") +
  scale_y_continuous(limits = c(y_min_Binary, y_max_Binary), position = "right") +
  theme_minimal(base_size = 16, base_family = "serif") +
  theme(
    axis.title.y.right = element_text(size = 14, face = "bold"),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# Combine the plots
plot_borus_Binary <- placebo_plot_Binary + treatment_plot_Binary +
  patchwork::plot_layout(widths = c(1, 1)) +
  patchwork::plot_annotation(
    theme = theme(
      plot.caption = element_text(size = 12, face = "italic", hjust = 0.5, margin = margin(t = 10))
    )
  )

# Event study plots for each model (individually)
plot_twfe_Binary <- create_methodology_plot(
  twfe_Binary_output, 
  ""
)

plot_cs_Binary <- create_methodology_plot(
  cs_Binary_output, 
  ""
)

plot_dcdh_Binary <- create_methodology_plot(
  dcdh_Binary_output, 
  ""
)

plot_dcdh_reversion_Binary <- create_methodology_plot(
  dcdh_reversion_Binary_output,
  ""
)

plot_dcdh_intensive_Binary <- create_methodology_plot(
  dcdh_intensive_Binary_output,
  ""
)

# Save each event study plot individually
ggsave("Binary_twfe_event_study.png", plot_twfe_Binary, width = 8, height = 6, dpi = 300)
ggsave("Binary_cs_event_study.png", plot_cs_Binary, width = 8, height = 6, dpi = 300)
ggsave("Binary_borus_event_study.png", plot_borus_Binary, width = 8, height = 6, dpi = 300)
ggsave("Binary_dcdh_event_study.png", plot_dcdh_Binary, width = 8, height = 6, dpi = 300)
ggsave("Binary_dcdh_reversion_event_study.png", plot_dcdh_reversion_Binary, width = 8, height = 6, dpi = 300)
ggsave("Binary_dcdh_intensive_event_study.png", plot_dcdh_intensive_Binary, width = 8, height = 6, dpi = 300)

# Calculate ATT estimates for each model
twfe_att_Binary <- felm(Binary_Total_articles ~ DTO_dummy_a | INEGI_CODE + Year | 0 | INEGI_CODE,
                        data = df.twfe_news)

coef_twfe_Binary <- c(twfe_att_Binary$coefficients)
se_twfe_Binary <- twfe_att_Binary$cse

df_twfe_att_Binary <- tibble::tibble(
  type = "TWFE", 
  coef = coef_twfe_Binary, 
  se = se_twfe_Binary,
  conf.low = coef_twfe_Binary - 1.96 * se_twfe_Binary,
  conf.high = coef_twfe_Binary + 1.96 * se_twfe_Binary
)

borus_att_Binary <- did_imputation(
  data = df.twfe_news, 
  yname = "Binary_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE"
) %>%
  dplyr::mutate(type = "Borusyak et al.") %>% 
  dplyr::select(
    type, 
    coef = estimate, 
    se = std.error, 
    conf.low, 
    conf.high
  )

agg_att_cs_Binary <- aggte(cs_Binary, type = "simple", na.rm = TRUE)

df_cs_att_Binary <- tibble::tibble(
  type = "Callaway & Sant'Anna", 
  coef = agg_att_cs_Binary$overall.att,
  se = agg_att_cs_Binary$overall.se, 
  conf.low = agg_att_cs_Binary$overall.att - 1.96 * agg_att_cs_Binary$overall.se,
  conf.high = agg_att_cs_Binary$overall.att + 1.96 * agg_att_cs_Binary$overall.se
)

coef_dcdh_Binary <- as.data.frame(dcdh_Binary$results$ATE)

df_dcdh_att_Binary <- coef_dcdh_Binary %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "de Chaisemartin & D'Haultfoeuille") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

coef_dcdh_reversion_Binary <- as.data.frame(dcdh_reversion_Binary$results$ATE)

df_dcdh_reversion_att_Binary <- coef_dcdh_reversion_Binary %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "dCDH (Reversion)") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

coef_dcdh_intensive_Binary <- as.data.frame(dcdh_intensive_Binary$results$ATE)

df_dcdh_intensive_att_Binary <- coef_dcdh_intensive_Binary %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "dCDH (Intensive)") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

# Combine ATT results and create plot
att_results_Binary <- dplyr::bind_rows(
  df_twfe_att_Binary,
  borus_att_Binary,
  df_cs_att_Binary,
  df_dcdh_att_Binary,
  df_dcdh_reversion_att_Binary,
  df_dcdh_intensive_att_Binary
)

att_results_Binary <- att_results_Binary %>%
  dplyr::filter(!is.na(coef))

att_plot_Binary <- create_results_plot(
  data = att_results_Binary, 
  x = "coef", 
  y = "type", 
  xmin = "conf.low", 
  xmax = "conf.high", 
  method_label = "Method"
)

# Save ATT plot
ggsave("Binary_att.png", att_plot_Binary, width = 8, height = 6, dpi = 300)

##########################################
# 4. CHEN AND ROTH (2024) TRANSFORMATION
##########################################

# TWFE model
plot_twfe_CR <- feols(LogLinear_Total_articles ~ i(Time_to_Treatment, Treatment_group, -1) | INEGI_CODE + Year,  
                      data = df.twfe_news, cluster = "INEGI_CODE")

twfe_CR_output <- tibble::tibble(
  rowname = names(plot_twfe_CR$coefficients),
  Estimate = plot_twfe_CR$coefficients,
  SE = plot_twfe_CR$se
) %>% 
  dplyr::mutate(
    rowname = gsub("Time_to_Treatment::", "", rowname),
    rowname = gsub(":Treatment_group", "", rowname),
    time = as.numeric(rowname),
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "TWFE"
  ) %>%
  dplyr::select(-rowname, -SE) %>%
  dplyr::filter(time >= -5 & time <= 5)

twfe_CR_output <- dplyr::bind_rows(
  twfe_CR_output,
  tibble::tibble(time = -1, Estimate = 0, LB_CI = 0, UB_CI = 0, Model = "TWFE")
)

# Callaway and Sant'Anna model
cs_CR <- att_gt(
  yname = "LogLinear_Total_articles",
  gname = "firstdto",
  idname = "INEGI_CODE",
  tname = "Year",
  xformla = ~1,
  control_group = "notyettreated",
  data = df.twfe_news,
   base_period = "universal"
)

cs_CR_agg <- aggte(cs_CR, type = "dynamic", bstrap = FALSE, cband = FALSE, na.rm = TRUE)

cs_CR_output <- tibble::tibble(
  time = cs_CR_agg$egt,
  Estimate = cs_CR_agg$att.egt,
  SE = cs_CR_agg$se.egt
) %>%
  dplyr::filter(time >= -5 & time <= 5) %>%
  dplyr::mutate(
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "CS"
  ) %>%
  dplyr::select(-SE)

# Borusyak estimation - Improved approach with separate pre/post estimations
# Post-treatment effects
borus_post_CR <- did_imputation(
  data = df.twfe_news, 
  yname = "LogLinear_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE",
  horizon = c(0:5)  # Only post-treatment effects
)

# Pre-treatment (placebo) effects
borus_pre_CR <- did_imputation(
  data = df.twfe_news, 
  yname = "LogLinear_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE",
  pretrends = c(-5:-1)  # Only pre-treatment effects
)

# Format results for plotting
borus_post_CR_output <- borus_post_CR %>%
  dplyr::mutate(
    time = as.numeric(term),
    Model = "Borusyak Post",
    Estimate = estimate,
    LB_CI = conf.low,
    UB_CI = conf.high
  ) %>%
  dplyr::select(time, Estimate, LB_CI, UB_CI, Model) %>% 
  dplyr::filter(time <= 5)

borus_pre_CR_output <- borus_pre_CR %>%
  dplyr::mutate(
    time = as.numeric(term),
    Model = "Borusyak Pre",
    Estimate = estimate,
    LB_CI = conf.low,
    UB_CI = conf.high
  ) %>%
  dplyr::select(time, Estimate, LB_CI, UB_CI, Model) %>% 
  dplyr::filter(time >= -5)

# de Chaisemartin and D'Haultfoeuille model - Standard
dcdh_CR <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "LogLinear_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "DTO_dummy_a",
  placebo = 50,
  effects = 50,
  graph_off = TRUE
)

dcdh_CR_output <- as.data.frame(dcdh_CR[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(Model = "dCDH") %>% 
  dplyr::mutate(time = time - 1) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# de Chaisemartin and D'Haultfoeuille - Treatment Reversion
dcdh_reversion_CR <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "LogLinear_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "Dich_Presence",
  effects = 50,
  placebo = 50,
  graph_off = TRUE,
  cluster = "INEGI_CODE"
)

dcdh_reversion_CR_output <- as.data.frame(dcdh_reversion_CR[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(
    Model = "dCDH Reversion",
    time = time - 1
  ) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# de Chaisemartin and D'Haultfoeuille - Intensive Margin
dcdh_intensive_CR <- did_multiplegt_dyn(
  df = df.twfe_news,
  outcome = "LogLinear_Total_articles",
  group = "INEGI_CODE",
  time = "Year",
  treatment = "Total_presence",
  effects = 50,
  placebo = 50,
  graph_off = TRUE,
  cluster = "INEGI_CODE"
)

dcdh_intensive_CR_output <- as.data.frame(dcdh_intensive_CR[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(
    Model = "dCDH Intensive",
    time = time - 1
  ) %>% 
  dplyr::filter(time >= -5 & time <= 5)

# Set min and max values for consistent y-axis in plots
y_min_CR <- min(c(min(borus_pre_CR_output$LB_CI), min(borus_post_CR_output$LB_CI)))
y_max_CR <- max(c(max(borus_pre_CR_output$UB_CI), max(borus_post_CR_output$UB_CI)))

# Create the placebo plot for CR
placebo_plot_CR <- ggplot(borus_pre_CR_output, aes(x = time, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "blue") +
  labs(x = "Time to Treatment",
       y = "Marginal Effect Coefficient") +
  scale_y_continuous(limits = c(y_min_CR, y_max_CR)) +
  theme_minimal(base_size = 16, base_family = "serif") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# Create the treatment effects plot for CR
treatment_plot_CR <- ggplot(borus_post_CR_output, aes(x = time, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "blue") +
  labs(x = "Time Since Treatment",
       y = "Marginal Effect Coefficient") +
  scale_y_continuous(limits = c(y_min_CR, y_max_CR), position = "right") +
  theme_minimal(base_size = 16, base_family = "serif") +
  theme(
    axis.title.y.right = element_text(size = 14, face = "bold"),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# Combine the plots
plot_borus_CR <- placebo_plot_CR + treatment_plot_CR +
  patchwork::plot_layout(widths = c(1, 1)) +
  patchwork::plot_annotation(
    theme = theme(
      plot.caption = element_text(size = 12, face = "italic", hjust = 0.5, margin = margin(t = 10))
    )
  )

# Event study plots for each model (individually)
plot_twfe_CR <- create_methodology_plot(
  twfe_CR_output, 
  ""
)

plot_cs_CR <- create_methodology_plot(
  cs_CR_output, 
  ""
)

plot_dcdh_CR <- create_methodology_plot(
  dcdh_CR_output, 
  ""
)

plot_dcdh_reversion_CR <- create_methodology_plot(
  dcdh_reversion_CR_output,
  ""
)

plot_dcdh_intensive_CR <- create_methodology_plot(
  dcdh_intensive_CR_output,
  ""
)

# Save each event study plot individually
ggsave("CR_twfe_event_study.png", plot_twfe_CR, width = 8, height = 6, dpi = 300)
ggsave("CR_cs_event_study.png", plot_cs_CR, width = 8, height = 6, dpi = 300)
ggsave("CR_borus_event_study.png", plot_borus_CR, width = 8, height = 6, dpi = 300)
ggsave("CR_dcdh_event_study.png", plot_dcdh_CR, width = 8, height = 6, dpi = 300)
ggsave("CR_dcdh_reversion_event_study.png", plot_dcdh_reversion_CR, width = 8, height = 6, dpi = 300)
ggsave("CR_dcdh_intensive_event_study.png", plot_dcdh_intensive_CR, width = 8, height = 6, dpi = 300)

# Calculate ATT estimates for each model
twfe_att_CR <- felm(LogLinear_Total_articles ~ DTO_dummy_a | INEGI_CODE + Year | 0 | INEGI_CODE,
                    data = df.twfe_news)

coef_twfe_CR <- c(twfe_att_CR$coefficients)
se_twfe_CR <- twfe_att_CR$cse

df_twfe_att_CR <- tibble::tibble(
  type = "TWFE", 
  coef = coef_twfe_CR, 
  se = se_twfe_CR,
  conf.low = coef_twfe_CR - 1.96 * se_twfe_CR,
  conf.high = coef_twfe_CR + 1.96 * se_twfe_CR
)

borus_att_CR <- did_imputation(
  data = df.twfe_news, 
  yname = "LogLinear_Total_articles", 
  gname = "firstdto",
  first_stage = ~ 0 | Year + INEGI_CODE,
  tname = "Year", 
  idname = "INEGI_CODE",
  cluster_var = "INEGI_CODE"
) %>%
  dplyr::mutate(type = "Borusyak et al.") %>% 
  dplyr::select(
    type, 
    coef = estimate, 
    se = std.error, 
    conf.low, 
    conf.high
  )

agg_att_cs_CR <- aggte(cs_CR, type = "simple", na.rm = TRUE)

df_cs_att_CR <- tibble::tibble(
  type = "Callaway & Sant'Anna", 
  coef = agg_att_cs_CR$overall.att,
  se = agg_att_cs_CR$overall.se, 
  conf.low = agg_att_cs_CR$overall.att - 1.96 * agg_att_cs_CR$overall.se,
  conf.high = agg_att_cs_CR$overall.att + 1.96 * agg_att_cs_CR$overall.se
)

coef_dcdh_CR <- as.data.frame(dcdh_CR$results$ATE)

df_dcdh_att_CR <- coef_dcdh_CR %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "de Chaisemartin & D'Haultfoeuille") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

coef_dcdh_reversion_CR <- as.data.frame(dcdh_reversion_CR$results$ATE)

df_dcdh_reversion_att_CR <- coef_dcdh_reversion_CR %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "dCDH (Reversion)") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

coef_dcdh_intensive_CR <- as.data.frame(dcdh_intensive_CR$results$ATE)

df_dcdh_intensive_att_CR <- coef_dcdh_intensive_CR %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "dCDH (Intensive)") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

# Combine ATT results and create plot
att_results_CR <- dplyr::bind_rows(
  df_twfe_att_CR,
  borus_att_CR,
  df_cs_att_CR,
  df_dcdh_att_CR,
  df_dcdh_reversion_att_CR,
  df_dcdh_intensive_att_CR
)

att_results_CR <- att_results_CR %>%
  dplyr::filter(!is.na(coef))

att_plot_CR <- create_results_plot(
  data = att_results_CR, 
  x = "coef", 
  y = "type", 
  xmin = "conf.low", 
  xmax = "conf.high", 
  method_label = "Method"
)

# Save ATT plot
ggsave("CR_att.png", att_plot_CR, width = 8, height = 6, dpi = 300)

##########################################
# SUMMARY: CREATE COMBINED PLOTS FOR ALL TRANSFORMATIONS
##########################################

# Create a combined plot showing all ATT estimates across transformations
# Add transformation identifier to each dataset
att_results_IHS$transformation <- "IHS"
att_results_Log1p$transformation <- "Log(1+x)"
att_results_Binary$transformation <- "Binary"
att_results_CR$transformation <- "Chen-Roth"

# Combine all ATT results
att_results_all <- dplyr::bind_rows(
  att_results_IHS,
  att_results_Log1p,
  att_results_Binary,
  att_results_CR
)

# Create faceted plot by transformation
att_plot_all <- ggplot(att_results_all, aes(x = coef, y = type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ transformation, scales = "free_x", ncol = 2) +
  labs(
    title = "Average Treatment Effects Across All Transformations and Methods",
    x = "Average Treatment Effect",
    y = "Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave("ATT_all_transformations_methods.png", att_plot_all, width = 14, height = 10, dpi = 300)
