# This script estimates survival models.

# Library
library("tidyverse")
library("survival")

# Paths
path.data.first.win <- "./careerEffect/data/first_win.RDS"
path.data.first.run <- "./careerEffect/data/first_run.RDS"
path.save.weibull.run.baseline <- "./careerEffect/data/weibull.run.baseline.RDS"
path.save.weibull.run.control <- "./careerEffect/data/weibull.run.control.RDS"
path.save.weibull.win.baseline <- "./careerEffect/data/weibull.win.baseline.RDS"
path.save.weibull.win.control <- "./careerEffect/data/weibull.win.control.RDS"

# Data
data.first.run <- readRDS(path.data.first.run)
data.first.win <- readRDS(path.data.first.win)

weibull.run.baseline <- survreg(
  data = data.first.run, 
  age.surv ~ 
    as.factor(assy) + as.factor(bcrat) + as.factor(sec) + as.factor(law) + as.factor(juku) + 
      as.factor(female) + as.factor(dynasty) + as.factor(kobo) + as.factor(is_pure_pr),  
  dist = "weibull"
)

weibull.run.control <- survreg(
  data = data.first.run, 
  age.surv ~ 
    as.factor(assy) + as.factor(bcrat) + as.factor(sec) + as.factor(law) + as.factor(juku) + 
      as.factor(female) + as.factor(dynasty) + as.factor(kobo) + as.factor(is_pure_pr) + 
      as.factor(assy):as.factor(dynasty) + as.factor(sec):as.factor(dynasty), 
  dist = "weibull"
)

weibull.win.baseline <- survreg(
  data = data.first.win,
  formula = age.surv ~
    as.factor(assy) + as.factor(bcrat) + as.factor(sec) + as.factor(law) + as.factor(juku) + 
      as.factor(female) + as.factor(dynasty) + as.factor(kobo) + as.factor(is_pure_pr), 
  dist = "weibull"
)
weibull.win.control <- survreg(
  data = data.first.win, 
  age.surv ~ 
    as.factor(assy) + as.factor(bcrat) + as.factor(sec) + as.factor(law) + as.factor(juku) + 
    as.factor(female) + as.factor(dynasty) + as.factor(kobo) + as.factor(is_pure_pr) + 
    as.factor(assy):as.factor(dynasty) + as.factor(sec):as.factor(dynasty), 
  dist = "weibull"
)

# Save
saveRDS(weibull.run.baseline, path.save.weibull.run.baseline)
saveRDS(weibull.run.control, path.save.weibull.run.control)
saveRDS(weibull.win.baseline, path.save.weibull.win.baseline)
saveRDS(weibull.win.control, path.save.weibull.win.control)
