# This script plots the estimation results of survival models.

# Library
library("cowplot")
library("tidyverse")
library("sandwich")
library("survival")
library("margins")
source("./careerEffect/code/util.R")

# Paths and parameters
path.data.first.run <- "./careerEffect/data/first_run.RDS"
path.weibull.run.baseline <- "./careerEffect/data/weibull.run.baseline.RDS"
path.weibull.run.control <- "./careerEffect/data/weibull.run.control.RDS"
path.weibull.win.baseline <- "./careerEffect/data/weibull.win.baseline.RDS"
path.weibull.win.control <- "./careerEffect/data/weibull.win.control.RDS"
path.save.effects <- "./careerEffect/figure/effects_survival.png"
path.save.survival <- "./careerEffect/figure/curve_survival.png"
list.map.attributes <- list(
  status_original = c(
    "intercept", "intercept*assy", "intercept*sec", "intercept*assy*sec", 
    "intercept*juku", "intercept*assy*juku", "intercept*sec*juku", 
    "intercept*assy*sec*juku", "intercept*dynasty", "intercept*assy*dynasty",
    "intercept*sec*dynasty", "intercept*assy*sec*dynasty", 
    "intercept*juku*dynasty", "intercept*assy*juku*dynasty", 
    "intercept*sec*juku*dynasty", "intercept*assy*sec*juku*dynasty", 
    "intercept*kobo", "intercept*assy*kobo", 
    "intercept*sec*kobo", "intercept*assy*sec*kobo", 
    "intercept*juku*kobo", "intercept*assy*juku*kobo",
    "intercept*sec*juku*kobo", "intercept*assy*sec*juku*kobo", 
    "intercept*dynasty*kobo", "intercept*assy*dynasty*kobo", 
    "intercept*sec*dynasty*kobo", "intercept*assy*sec*dynasty*kobo", 
    "intercept*juku*dynasty*kobo", "intercept*assy*juku*dynasty*kobo", 
    "intercept*sec*juku*dynasty*kobo", "intercept*assy*sec*juku*dynasty*kobo"
  ), 
  status_plot = c(
    "Baseline", "Local", "Secretary", "Secretary; Local", 
    "Juku", "Juku; Local", "Secretary; Juku", 
    "Secretary; Juku; Local", "Successor", "Local; Successor", 
    "Secretary; Successor", "Secretary; Local; Successor",
    "Juku; Successor", "Juku; Local; Successor", 
    "Secretary; Juku; Successor", "Secretary; Juku; Local; Successor", 
    "Open", "Local; Open", 
    "Secretary; Open", "Secretary; Local; Open", 
    "Juku; Open", "Juku; Local; Open", 
    "Secretary; Juku; Open", "Secretary; Juku; Local; Open",
    "Successor; Open", "Local; Successor; Open", 
    "Secretary; Successor; Open", "Secretary; Local; Successor; Open",
    "Juku; Successor; Open", "Juku; Local; Successor; Open", 
    "Secretary; Juku; Successor; Open", "Secretary; Juku; Local; Successor; Open"
  )
)
kAlpha <- 0.05
kZValue <- qnorm(1 - kAlpha / 2)
set.seed(1010142)

# Data and Survival objects
data.first.run <- readRDS(path.data.first.run)
weibull.run.baseline <- readRDS(path.weibull.run.baseline)
weibull.run.control <- readRDS(path.weibull.run.control)
weibull.win.baseline <- readRDS(path.weibull.win.baseline)
weibull.win.control <- readRDS(path.weibull.win.control)

# Effects plot
data_new = expand.grid(
  intercept = 1, 
  assy = c(0, 1), 
  bcrat = 0, 
  sec = c(0, 1),
  law = 0, 
  juku = c(0, 1), 
  Secretary = 0, 
  dynasty = c(0, 1), 
  kobo = c(0, 1), 
  is_pure_pr = 0
)
stats_weibull_run_control <- expWeibull(
  model_weibull = weibull.run.control,
  data_new = data_new
) %>% 
  select(-c(mean_boot, se)) %>% 
  mutate(
    # rename what's in "list.map.attributes$status_original" to "list.map.attributes$status_plot"
    status = list.map.attributes$status_plot[match(.$status, list.map.attributes$status_original)]
  )
stats_weibull_win_control <- expWeibull(
  model_weibull = weibull.win.control,
  data_new = data_new
) %>% 
  select(-c(mean_boot, se)) %>% 
  mutate(
    # rename what's in "list.map.attributes$status_original" to "list.map.attributes$status_plot"
    status = list.map.attributes$status_plot[match(.$status, list.map.attributes$status_original)]
  )
# Plot
effects.weibull.run.control <- stats_weibull_run_control %>% 
  ggplot() + 
  geom_point(
    aes(
      x = status, 
      y = mean
    )
  ) + 
  geom_errorbar(
    aes(
      x = status, 
      ymin = ci_lower, 
      ymax = ci_upper
    ), 
    linewidth = 0.2
  ) + 
  labs(
    x = NULL, 
    y = NULL
  ) + 
  coord_flip() +  # rotate
  theme_minimal() +
  theme(
    axis.text.y = element_text(
      size = 8, 
      hjust = 0
    )
  )
effects.weibull.win.control <- stats_weibull_win_control %>% 
  ggplot() + 
  geom_point(
    aes(
      x = status, 
      y = mean
    )
  ) + 
  geom_errorbar(
    aes(
      x = status, 
      ymin = ci_lower, 
      ymax = ci_upper
    ), 
    linewidth = 0.2
  ) + 
  labs(
    x = NULL, 
    y = "Years past since turning 25"
  ) + 
  coord_flip() +  # rotate
  theme_minimal() +
  theme(
    axis.text.y = element_text(
      size = 8, 
      hjust = 0
    )
  )

plot_grid(
  effects.weibull.run.control, 
  effects.weibull.win.control, 
  ncol = 1
) %>% 
  ggsave(
    filename = path.save.effects,
    width = 6.6,
    height = 7.8,
    units = "in"
  )

# Survival curves
# Secretary * successor
plot.sec.dynasty <- weibull.run.control %>% 
  predict(
    newdata = expand.grid(
      assy = 0, 
      bcrat = 0, 
      sec = c(0, 1),
      female = 0, 
      law = 0, 
      juku = 0, 
      Secretary = 0, 
      dynasty = c(0, 1), 
      kobo = 0, 
      is_pure_pr = 0
    ), 
    type = "quantile",
    p = seq(.01,.99,by=.01)
  ) %>% 
  t() %>% 
  as.data.frame() %>%
  rename(
    "Baseline" = "V1", 
    "Secretary" = "V2", 
    "Successor" = "V3",
    "Secretary; successor" = "V4"
  ) %>% 
  mutate(prob = seq(.99, .01, by = -.01)) %>% 
  pivot_longer(
    cols = -prob,
    names_to = "assy_level",
    values_to = "prediction"
  ) %>% 
  ggplot() +
  geom_line(aes(
    x = prediction, 
    y = prob, 
    linetype = assy_level
  )) +  
  scale_linetype_manual(
    name = NULL,  # Change to your preferred title
    values = c("solid", "dashed", "dotted", "dotdash")
  ) + 
  guides(
    linetype = guide_legend(
      title = NULL, 
      nrow = 2
    )
  ) +
  labs(
    x = NULL, 
    y = "Probability of Survival", 
    group = "Characteristics"
  ) +
  theme_minimal() + 
  theme(
    legend.text.align = 0, 
    legend.position = c(.7, .8)
  )

# successor * local legislator
plot.assy.dynasty <- weibull.run.control %>% 
  predict(
    newdata = expand.grid(
      assy = c(0, 1), 
      bcrat = 0, 
      sec = 0,
      female = 0, 
      law = 0, 
      juku = 0, 
      Secretary = 0, 
      dynasty = c(0, 1), 
      kobo = 0, 
      is_pure_pr = 0
    ), 
    type = "quantile",
    p = seq(.01,.99,by=.01)
  ) %>% 
  t() %>% 
  as.data.frame() %>%
  rename(
    "Baseline" = "V1", 
    "Local legislator" = "V2",
    "Successor" = "V3",
    "Local legislator; successor" = "V4"
  ) %>% 
  mutate(prob = seq(.99, .01, by = -.01)) %>% 
  pivot_longer(
    cols = -prob,
    names_to = "assy_level",
    values_to = "prediction"
  ) %>% 
  ggplot() +
  geom_line(aes(
    x = prediction, 
    y = prob, 
    linetype = assy_level
  )) +
  scale_linetype_manual(
    name = NULL,  # Change to your preferred title
    values = c("solid", "dashed", "dotted", "dotdash")
  ) +
  guides(
    linetype = guide_legend(
      title = NULL, 
      nrow = 2
    )
  ) +
  labs(
    x = "Years Past Since Turning 25", 
    y = "Probability of Survival", 
    group = "Characteristics"
  ) +
  theme_minimal() + 
  theme(
    legend.text.align = 0, 
    legend.position = c(.7, .8)
  )

plot_grid(
  plot.sec.dynasty, 
  plot.assy.dynasty, 
  nrow = 2
)

ggsave(
  filename = path.save.survival, 
  plot = last_plot(),
  width = 6.6,
  height = 4.4,
  units = "in"
)
