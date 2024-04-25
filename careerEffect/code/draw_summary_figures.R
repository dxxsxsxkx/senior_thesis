# This script draws and saves figures. 

# Library
library(tidyverse)
library(cowplot)

# Paths
path.data.candidate <- "./data/dataverse_files/Reedsmith_complete.csv"
path.data.first.win <- "./careerEffect/data/first_win.RDS"
path.data.first.run <- "./careerEffect/data/first_run.RDS"

# Data
data.first.run <- readRDS(path.data.first.run)
data.first.win <- readRDS(path.data.first.win)

# Parameters


# inspect the age distribution of first-time run / entrance
# first run
plot.first.run <- 
  data.first.run %>%
    ggplot(
      aes(
        x = sort(age.surv[,1], decreasing = TRUE),
        y = 1:nrow(data.first.run) / nrow(data.first.run),
        linetype = "Empirical Data"  # Assign a name for legend
      )
    ) + 
    geom_line() + 
    geom_line(
      data = data.frame(
        x = seq(
          min(data.first.run$age.surv[,1]),  
          max(data.first.run$age.surv[,1]), 
          length.out = nrow(data.first.run)
        )
      ),
      aes(
        x = x, 
        y = pweibull(x, shape = 2.5, scale = 25, lower.tail = FALSE),
        linetype = "Weibull Function"  # Assign a different name for this line
      )
    ) +
    guides(
      linetype = guide_legend(
        title = NULL, 
        nrow = 2
      )
    ) +
    scale_linetype_manual(
      name = NULL,  # Change to your preferred title
      values = c("solid", "dashed"),
      labels = c("Empirical Data", "Weibull (2.5, 25)")
    ) +
    scale_x_continuous(
      breaks = seq(0, 100, by = 10), 
      limits = c(0, max(data.first.run$age.surv[,1]))
    ) + 
    labs(
      x = NULL, 
      y = "Proportion: First-time Run"
    ) + 
    theme_minimal(
      base_size = 13
    ) + 
    theme(
      legend.position = c(.8, .8)
    )

# win
plot.first.win <- 
  data.first.win %>%
    ggplot(
      aes(
        x = sort(age.surv[,1], decreasing = TRUE),
        y = 1:nrow(data.first.win) / nrow(data.first.win),
        linetype = "Empirical Data"  # Assign a name for legend
      )
    ) + 
    geom_line() + 
    geom_line(
      data = data.frame(
        x = seq(
          min(data.first.win$age.surv[,1]),  
          max(data.first.win$age.surv[,1]), 
          length.out = nrow(data.first.win)
        )
      ),
      aes(
        x = x, 
        y = pweibull(x, shape = 2.7, scale = 27, lower.tail = FALSE),
        linetype = "Weibull Function"  # Assign a different name for this line
      )
    ) +  
    scale_linetype_manual(
      name = NULL,  # Change to your preferred title
      values = c("solid", "dashed"),
      labels = c("Empirical Data", "Weibull (2.7, 27)")
    ) +
    scale_x_continuous(
      breaks = seq(0, 100, by = 10), 
      limits = c(0, max(data.first.win$age.surv[,1]))
    ) + 
    labs(
      x = "Age since the age of 25", 
      y = "Proportion: First-time Win"
    ) + 
    theme_minimal(
      base_size = 13
    ) + 
    theme(
      legend.position = c(.8, .8)
    )

plot_grid(
  plotlist = list(plot.first.run, plot.first.win),
  nrow = 2, 
  ncol = 1
)

ggsave(
 filename = "./careerEffect/figure/first_run_win.png", 
 plot = last_plot(),
 width = 6,
 height = 4.5,
 units = "in"
)
