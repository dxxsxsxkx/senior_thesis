# Library
library("tidyverse")
library("sandwich")
library("survival")
library("texreg")
source("./careerEffect/code/util.R")

# Paths and parameters
path.weibull.run.baseline <- "./careerEffect/data/weibull.run.baseline.RDS"
path.weibull.run.control <- "./careerEffect/data/weibull.run.control.RDS"
path.weibull.win.baseline <- "./careerEffect/data/weibull.win.baseline.RDS"
path.weibull.win.control <- "./careerEffect/data/weibull.win.control.RDS"
path.save <- "./careerEffect/table/table_regression_run.tex"

# Survival objects
weibull.run.baseline <- readRDS(path.weibull.run.baseline)
weibull.run.control <- readRDS(path.weibull.run.control)
weibull.win.baseline <- readRDS(path.weibull.win.baseline)
weibull.win.control <- readRDS(path.weibull.win.control)

# Texreg objects
texreg.run.baseline <- createTexreg(
  coef.names = weibull.run.baseline$coef %>% names(), 
  coef = weibull.run.baseline$coef,
  se = sqrt(diag(vcov(weibull.run.baseline)[1:(length(weibull.run.baseline$coef)), 1:(length(weibull.run.baseline$coef))])),
  pvalues = 2 * (1 - pnorm(abs(weibull.run.baseline$coef / sqrt(diag(vcov(weibull.run.baseline)[1:(length(weibull.run.baseline$coef)), 1:(length(weibull.run.baseline$coef))]))))),
  gof.names = c("Num. obs", "Log Likelihood", "AIC"),
  gof = c(
    length(weibull.run.baseline$linear.predictors), 
    weibull.run.baseline$loglik[2], 
    AIC(weibull.run.baseline)
  ),
  gof.decimal = c(FALSE, TRUE, TRUE)
)
texreg.run.control <- createTexreg(
  coef.names = weibull.run.control$coef %>% names(), 
  coef = weibull.run.control$coef,
  se = sqrt(diag(vcov(weibull.run.control)[1:(length(weibull.run.control$coef)), 1:(length(weibull.run.control$coef))])),
  pvalues = 2 * (1 - pnorm(abs(weibull.run.control$coef / sqrt(diag(vcov(weibull.run.control)[1:(length(weibull.run.control$coef)), 1:(length(weibull.run.control$coef))]))))),
  gof.names = c("Num. obs", "Log Likelihood", "AIC"),
  gof = c(
    length(weibull.run.control$linear.predictors), 
    weibull.run.control$loglik[2], 
    AIC(weibull.run.control)
  ),
  gof.decimal = c(FALSE, TRUE, TRUE)
)
texreg.win.baseline <- createTexreg(
  coef.names = weibull.win.baseline$coef %>% names(), 
  coef = weibull.win.baseline$coef,
  se = sqrt(diag(vcov(weibull.win.baseline)[1:(length(weibull.win.baseline$coef)), 1:(length(weibull.win.baseline$coef))])),
  pvalues = 2 * (1 - pnorm(abs(weibull.win.baseline$coef / sqrt(diag(vcov(weibull.win.baseline)[1:(length(weibull.win.baseline$coef)), 1:(length(weibull.win.baseline$coef))]))))),
  gof.names = c("Num. obs", "Log Likelihood", "AIC"),
  gof = c(
    length(weibull.win.baseline$linear.predictors), 
    weibull.win.baseline$loglik[2], 
    AIC(weibull.win.baseline)
  ),
  gof.decimal = c(FALSE, TRUE, TRUE)
)
texreg.win.control <- createTexreg(
  coef.names = weibull.win.control$coef %>% names(), 
  coef = weibull.win.control$coef,
  se = sqrt(diag(vcov(weibull.win.control)[1:(length(weibull.win.control$coef)), 1:(length(weibull.win.control$coef))])),
  pvalues = 2 * (1 - pnorm(abs(weibull.win.control$coef / sqrt(diag(vcov(weibull.win.control)[1:(length(weibull.win.control$coef)), 1:(length(weibull.win.control$coef))]))))),
  gof.names = c("Num. obs", "Log Likelihood", "AIC"),
  gof = c(
    length(weibull.win.control$linear.predictors), 
    weibull.win.control$loglik[2], 
    AIC(weibull.win.control)
  ),
  gof.decimal = c(FALSE, TRUE, TRUE)
)

# Table
table.regression <- texreg(
  list(texreg.run.baseline, texreg.run.control, texreg.win.baseline, texreg.win.control), 
  custom.model.names = c("Baseline", "Control", "Baseline", "Control"),
  custom.header = list("First Candidacy" = 1:2, "First Win" = 3:4),
  custom.coef.map = list(
    "(Intercept)" = NA, 
    "as.factor(assy)1" = "Local Assembly",
    "as.factor(bcrat)1" = "Bureaucrat", 
    "as.factor(sec)1" = "Secretary",
    "as.factor(law)1" = "Lawyer",
    "as.factor(juku)1" = "Juku",
    "as.factor(female)1" = "Female", 
    "as.factor(dynasty)1" = "Successor", 
    "as.factor(kobo)1" = "Open recruitment", 
    "as.factor(is_pure_pr)1" = "Pure PR candidate", 
    "as.factor(assy)1:as.factor(dynasty)1" = "Local Assembly × Successor",
    "as.factor(sec)1:as.factor(dynasty)1" = "Secretary × Successor"
  ), 
  digits = 3, 
  custom.note = paste0(
    "\n\\item %stars.", 
    "\n\\item \\textit{Note}. Coefficients are changes in \\textit{survival time},", 
    "i.e., years spent before candidates' first candidacy (first and second columns) and election (third and fourth columns)."
  ), 
  caption = "Weibull Regression: Years Spent Before First Candidacy", 
  threeparttable = TRUE, 
  label = "table:weibull", 
  single.row = FALSE, 
  booktabs = TRUE, 
  dcolumn = TRUE, 
  use.packages = FALSE,
  float.pos = "H", 
  scalebox = 0.9
)

# Output
writeLines(table.regression, path.save)
