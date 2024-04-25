# Libraries
library("knitr")
library("tidyverse")
library("lmtest")
library("plm")
library("lme4")
library("texreg")
library("estimatr")
library("kableExtra")
library("MatchIt")
library("vtable")
library("mice")
library("mitools")

# data processing
# create two new variables
data <- read_csv("data/dataverse_files/Reedsmith_19472020.csv") %>% 
  mutate(
    ku_voteshare = ku_vote / ku_totvote * 100, 
    inc = as.factor(ifelse(inc == 0, 0, 1)), # incumbency status
    ldp = as.factor(ifelse(party_en == "LDP", 1, 0)), # LDP dummy
    seshu = as.factor(seshu)
  )

# smaller dataset 
# focusing on candidates after 1996 in snv constituencies
# 1996: electoral reform
snv <- data %>% 
  filter(year >= 1996) %>% 
  filter(!str_detect(kuname, "bloc")) %>%  # remove pure PR candidates
  dplyr::select(
    ku_voteshare, age, exp, ldp, seshu, inc, kuname, year, inc, pid
  )


# matching prep
snv$treat <- NA
snv$thres <- NA

for (t in unique(snv$year)) { # for each election year
  snv_yearly <- snv[snv$year == t, ] # subset data
  kMeanAge <- mean(snv_yearly$age, na.rm = TRUE) # compute mean age of candidates
  if(length(snv_yearly$pid) > 1){
    kSdAge <- sd(snv_yearly$age, na.rm = TRUE) #compute sd of candidates' aage
    kThres <- kMeanAge - kSdAge # take (mean - 1sd)
  }else{
    kThres <- kMeanAge # sd cannot be calculated for 1 sample
  }
  snv$thres[snv$year == t] <- kThres # substitute
}
snv$treat <- ifelse(snv$age <= snv$thres, 1, 0) # final treatment variable


# missing variable treatment
# list-wise deletion
snv.noNA <- snv %>% 
  filter(is.na(ku_voteshare) == FALSE, 
         is.na(age) == FALSE, # age missing
         is.na(exp) == FALSE) # exp / ldp / seshu missing

# imputation
# two variables with missingness: exp / seshu
snv.imp <- mice(
  data = snv, 
  maxit = 0
)
post <- snv.imp$post
# squeeze imputed values of expenditure to non-negative
post["exp"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, max(snv$exp)))"
snv.imp <- mice(
  data = snv, 
  m = 10, 
  method = c("", "", "norm", "", "polyreg", "", "", "", "", "", ""), 
  post = post
)
snv.imps = vector(mode = "list", length = 10)
for (i in 1:10) {
  snv.imps[[i]] <- complete(snv.imp, i)
}


# summary stats
# add summaries of inc, exp, ldp, seshu elsewhere
sumtable(
  data = snv.noNA, 
  out = "latex", 
  vars = c("ku_voteshare", "age"), 
  labels = c("Vote share", "Age"), 
  summ = 
    c(
      'mean(x)', 'sd(x)', 'min(x)', 'pctile(x)[25]', 
      'median(x)', 'pctile(x)[75]','max(x)'
    ), 
  summ.names = 
    c(
      'Mean', 'Std. Dev.', 'Min', 'Pctl. 25', 
      'Median', 'Pctl. 75', 'Max'
    ), 
  file = "figure/tab.summary"
) 


# homoskedasticity test
# prep
fit.test.hsk <- lm(data = snv.noNA, ku_voteshare ~ age)
snv.noNA$resid.age <- fit.test.hsk$residuals
# visualisation
snv.noNA %>% 
  ggplot(aes(x = age, y = resid.age)) + 
  geom_point(col = "blue") + 
  geom_abline(slope = 0) + 
  labs(
    title = "homoskedasticity check", 
    x = "age", 
    y = "residual"
  )
ggsave("figure/fig.homoskedasticity.pdf")
dev.off()

# Breutsch-Pagan test
lmtest::bptest(fit.test.hsk)


# regression
imp.ln <- with(
  snv.imp, 
  lm_robust(
    data = snv.imps[[i]], se_type = "HC1", 
    ku_voteshare ~ age
  )
)
imp.ln.cv <- with(
  snv.imp, 
  lm_robust(
    data = snv.imps[[i]], se_type = "HC1", 
    ku_voteshare ~ age + inc + log(exp + 1) + ldp + seshu
  )
)
imp.ln.it <- with(
  snv.imp, 
  lm_robust(
    data = snv.imps[[i]], se_type = "HC1", 
    ku_voteshare ~ age + inc + log(exp + 1) + ldp + seshu + 
      age*inc + age*(log(exp + 1)) + 
      (log(exp + 1))*ldp + (log(exp + 1))*seshu
  )
)
imp.ln.fx.cv <- with(
  snv.imp, 
  plm(
    data = snv.imps[[i]], effect = "twoways", model = "within",
    index = c("kuname", "year"), 
    ku_voteshare ~ age + inc + log(exp + 1) + ldp + seshu
  )
)
imp.ln.fx.it <- with(
  snv.imp, 
  plm(
    data = snv.imps[[i]], effect = "twoways", model = "within",
    index = c("kuname", "year"), 
    ku_voteshare ~ age + inc + log(exp + 1) + ldp + seshu + 
      age*inc + age*(log(exp + 1)) + 
      (log(exp + 1))*ldp + (log(exp + 1))*seshu
  )
)
result.ln <- summary(pool(imp.ln))
result.ln.cv <- summary(pool(imp.ln.cv))
result.ln.it <- summary(pool(imp.ln.it))
result.ln.fx.cv <- summary(pool(imp.ln.fx.cv))
result.ln.fx.it <- summary(pool(imp.ln.fx.it))


# regression output
texreg.ln <- createTexreg(
  coef.names = as.character(result.ln$term), 
  coef = result.ln$estimate, 
  se = result.ln$std.error, 
  pvalues = result.ln$p.value, 
  gof.names = "Num. obs.", 
  gof = as.integer(8976)
)
texreg.ln.cv = createTexreg(
  coef.names = as.character(result.ln.cv$term), 
  coef = result.ln.cv$estimate, 
  se = result.ln.cv$std.error, 
  pvalues = result.ln.cv$p.value, 
  gof.names = "Num. obs.", 
  gof = as.integer(8976)
)
texreg.ln.it = createTexreg(
  coef.names = as.character(result.ln.it$term), 
  coef = result.ln.it$estimate, 
  se = result.ln.it$std.error, 
  pvalues = result.ln.it$p.value, 
  gof.names = "Num. obs.", 
  gof = as.integer(8976)
)
texreg.ln.fx.cv = createTexreg(
  coef.names = as.character(result.ln.fx.cv$term), 
  coef = result.ln.fx.cv$estimate, 
  se = result.ln.fx.cv$std.error, 
  pvalues = result.ln.fx.cv$p.value, 
  gof.names = "Num. obs.", 
  gof = as.integer(8976)
)
texreg.ln.fx.it = createTexreg(
  coef.names = as.character(result.ln.fx.it$term), 
  coef = result.ln.fx.it$estimate, 
  se = result.ln.fx.it$std.error, 
  pvalues = result.ln.fx.it$p.value, 
  gof.names = "Num. obs.", 
  gof = as.integer(8976)
)
tab.linear <- texreg(
  list(
    texreg.ln, texreg.ln.cv, texreg.ln.it, texreg.ln.fx.cv, texreg.ln.fx.it
  ), 
  custom.coef.map = 
    list("(Intercept)" = NA, 
         "age" = "Age", 
         "inc1" = "Incumbency", "log(exp + 1)" = "Expenditure: log", 
         "ldp1" = "LDP", "seshu1" = "Inheritance", 
         "age:inc1" = "Age * Incumbency", 
         "age:log(exp + 1)" = "Age * Expenditure",  
         "log(exp + 1):ldp1" = "Expenditure * LDP", 
         "log(exp + 1):seshu1" = "Expenditure * Inheritance"), 
  custom.gof.rows = 
    list("Fixed Effects" = c(rep("No", 3), rep("Yes", 2))),
  scalebox = 0.9, 
  ci.force = FALSE, 
  table = TRUE, 
  center = TRUE,
  booktabs = TRUE, 
  dcolumn = TRUE, 
  use.packages = FALSE,
  return.string = TRUE, 
  digits = 3, 
  caption = "Linear models"
)
writeLines(tab.linear, "figure/tab.linear.tex")

# matching
m.output <- with(
  snv.imp, 
  matchit(
    method = "nearest", 
    distance = "glm", link = "logit", 
    discard = "both", # arbitrary
    caliper = 0.2, # arbitrary
    replace = TRUE, # arbitrary
    ratio = 3, # arbitrary
    treat ~ inc + log(exp + 1) + ldp + seshu
  )
)
m.results <- m.output$analyses

# TODO: pull out matched data, construct models for the post analysis, 
# and put estimates together
m.fits <- vector(mode = "list", length = 10)
m.fits.cv <- vector(mode = "list", length = 10)
m.fits.it <- vector(mode = "list", length = 10)
m.fits.fx.cv <- vector(mode = "list", length = 10)
m.fits.fx.it <- vector(mode = "list", length = 10)
for (i in 1:10) {
  m.data <- match.data(m.results[[i]], data = snv.imps[[i]])
  m.fits[[i]] <- lm_robust(
    data = m.data, se_type = "HC1", 
    ku_voteshare ~ treat
  )
  m.fits.cv[[i]] <- lm_robust(
    data = m.data, se_type = "HC1", 
    ku_voteshare ~ treat + inc + log(exp + 1) + ldp + seshu
  )
  m.fits.it[[i]] <- lm_robust(
    data = m.data, se_type = "HC1", 
    ku_voteshare ~ treat + inc + log(exp + 1) + ldp + seshu + 
      treat*inc + treat*(log(exp + 1)) + 
      (log(exp + 1))*ldp + (log(exp + 1))*seshu
  )
  m.fits.fx.cv[[i]] <- plm(
    data = m.data, effect = "twoways", model = "within",
    index = c("kuname", "year"), 
    ku_voteshare ~ treat + inc + log(exp + 1) + ldp + seshu
  )
  m.fits.fx.it[[i]] <- plm(
    data = m.data, effect = "twoways", model = "within",
    index = c("kuname", "year"), 
    ku_voteshare ~ treat + inc + log(exp + 1) + ldp + seshu + 
      treat*inc + treat*(log(exp + 1)) + 
      (log(exp + 1))*ldp + (log(exp + 1))*seshu
  )
}
# first model
m.coef <- lapply(m.fits, coef)
m.vcov <- lapply(m.fits, vcov)
results <- MIcombine(m.coef, m.vcov)
# second
m.coef.cv <- lapply(m.fits.cv, coef)
m.vcov.cv <- lapply(m.fits.cv, vcov)
results.cv <- MIcombine(m.coef.cv, m.vcov.cv)
# third
m.coef.it <- lapply(m.fits.it, coef)
m.vcov.it <- lapply(m.fits.it, vcov)
results.it <- MIcombine(m.coef.it, m.vcov.it)
# four
m.coef.fx.cv <- lapply(m.fits.fx.cv, coef)
m.vcov.fx.cv <- lapply(m.fits.fx.cv, vcov)
results.fx.cv <- MIcombine(m.coef.fx.cv, m.vcov.fx.cv)
# five
m.coef.fx.it <- lapply(m.fits.fx.it, coef)
m.vcov.fx.it <- lapply(m.fits.fx.it, vcov)
results.fx.it <- MIcombine(m.coef.fx.it, m.vcov.fx.it)

# retained samples
kMatchedN <- 0
for (i in 1:10) {
  kMatchedN <- kMatchedN + m.fits[[i]]$nobs
  if (i == 10) {
    kMatchedN <- kMatchedN / i
  }
}

# matching summary
summary.matched <- summary(m.results[[1]], un = TRUE)


# matching balancing check
# show absolute standarized mean difference
fig.matched <- plot(
  summary.matched, 
  xlim = c(0, 0.5), 
  main = "balancing check"
)

# show QQ plot
plot(m.results[[1]])


# post-match analysis
mreg <- createTexreg(
  coef.names = names(results$coefficients), 
  coef = results$coefficients, 
  se = c(sqrt(results$variance[1, 1]), sqrt(results$variance[2, 2])), 
  gof.names = "Num. obs.", 
  gof = kMatchedN
)
mreg.cv <- createTexreg(
  coef.names = names(results.cv$coefficients), 
  coef = results.cv$coefficients, 
  se = c(sqrt(results.cv$variance[1, 1]), sqrt(results.cv$variance[2, 2])), 
  gof.names = "Num. obs.", 
  gof = kMatchedN
)
mreg.it <- createTexreg(
  coef.names = names(results.it$coefficients), 
  coef = results.it$coefficients, 
  se = c(sqrt(results.it$variance[1, 1]), sqrt(results.it$variance[2, 2])), 
  gof.names = "Num. obs.", 
  gof = kMatchedN
)
mreg.fx.cv <- createTexreg(
  coef.names = names(results.fx.cv$coefficients), 
  coef = results.fx.cv$coefficients, 
  se = c(sqrt(results.fx.cv$variance[1, 1]), sqrt(results.fx.cv$variance[2, 2])), 
  gof.names = "Num. obs.", 
  gof = kMatchedN
)
mreg.fx.it <- createTexreg(
  coef.names = names(results.fx.it$coefficients), 
  coef = results.fx.it$coefficients, 
  se = c(sqrt(results.fx.it$variance[1, 1]), sqrt(results.fx.it$variance[2, 2])), 
  gof.names = "Num. obs.", 
  gof = kMatchedN
)

# output
tab.match <- texreg(
  list(mreg, mreg.cv, mreg.it, mreg.fx.cv, mreg.fx.it), 
  custom.coef.map = 
    list("(Intercept)" = NA, "treat" = "Treatment", "inc1" = "Incumbency", 
         "log(exp + 1)" = "Expenditure: log", 
         "ldp1" = "LDP", "seshu1" = "Inheritance", 
         "treat:inc1" = "Treatment * Incumbency", 
         "treat:log(exp + 1)" = "Treatment * Expenditure",  
         "log(exp + 1):ldp1" = "Expenditure * LDP", 
         "log(exp + 1):seshu1" = "Expenditure * Inheritance"), 
  custom.gof.rows = 
    list("Fixed Effects" = c(rep(c(rep("No", 3), rep("Yes", 2)), 1))),  
  custom.note = 
    (
      "\n\\item Treatment is to be younger than 
      (Average age of candidates in a particular election) - 
      (Standard deviation of candidates' age in that election).\n"
    ), 
  table = TRUE, 
  center = TRUE,
  booktabs = TRUE, 
  dcolumn = TRUE, 
  use.packages = FALSE,
  return.string = TRUE,
  threeparttable = TRUE, 
  digits = 3, 
  caption = "Post-matching analysis"
)
writeLines(tab.match, "figure/tab.matched.tex")







