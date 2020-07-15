## Subjective happiness ~ structural MRI data
# Created by Yong Zhang, 6/11/2020

# set working directory
setwd("~/HRV/MRI_Structural")

# load libraries
library("stringi")
library("stargazer")

# import and merge the data
datafile <- read.csv("2020_06_segmented_volume_YA_combined.csv")
datafile <- as.data.frame(datafile)
as.factor(datafile$Gender_1M_0F)

# replace NA with sample mean
for(i in 1:ncol(datafile)){
  datafile[,i]=ifelse(is.na(datafile[,i]),
                  ave(datafile[,i],FUN=function(y) mean(y, na.rm = TRUE)),
                  datafile[,i])
}

# models for pre-training data
var_names <- colnames(datafile)
var_pre <- var_names[stri_detect(var_names, regex = 'pre_')]
var_post <- var_names[stri_detect(var_names, regex = 'post_')]
var_pc <- var_names[stri_detect(var_names, regex = 'pc1_')]

dependents_name <- c()
models_SAI <- list()
models_TAI <- list()
models_CESD <- list()
models_POMS <- list()

for (ii in 1:length(var_pre)) {
  models_SAI[[ii]] <- lm(datafile[,var_pre[ii]] ~ SAI_w2 + Age + Gender_1M_0F + pre_TotalGrayVol, data = datafile)
  models_TAI[[ii]] <- lm(datafile[,var_pre[ii]] ~ TAI_w2 + Age + Gender_1M_0F + pre_TotalGrayVol, data = datafile)
  models_CESD[[ii]] <- lm(datafile[,var_pre[ii]] ~ CESD_w2 + Age + Gender_1M_0F + pre_TotalGrayVol, data = datafile)
  models_POMS[[ii]] <- lm(datafile[,var_pre[ii]] ~ POMS_w2 + Age + Gender_1M_0F + pre_TotalGrayVol, data = datafile)
  dependents_name[ii] = stri_replace_all_fixed(var_pre[ii], "_", "-")
}
stargazer(models_POMS[1:20], column.labels = dependents_name[1:20], dep.var.labels.include = FALSE, model.names = TRUE, type = "text", ci = TRUE, ci.level = 0.95, report = "vcstp*", out = "POMS_pre1.txt")
stargazer(models_POMS[21:40], column.labels = dependents_name[21:40], dep.var.labels.include = FALSE, model.names = TRUE, type = "text", ci = TRUE, ci.level = 0.95, report = "vcstp*", out = "POMS_pre2.txt")
stargazer(models_POMS[41:60], column.labels = dependents_name[41:60], dep.var.labels.include = FALSE, model.names = TRUE, type = "text", ci = TRUE, ci.level = 0.95, report = "vcstp*", out = "POMS_pre3.txt")
stargazer(models_POMS[61:80], column.labels = dependents_name[61:80], dep.var.labels.include = FALSE, model.names = TRUE, type = "text", ci = TRUE, ci.level = 0.95, report = "vcstp*", out = "POMS_pre4.txt")
stargazer(models_POMS[81:100], column.labels = dependents_name[81:100], dep.var.labels.include = FALSE, model.names = TRUE, type = "text", ci = TRUE, ci.level = 0.95, report = "vcstp*", out = "POMS_pre5.txt")
stargazer(models_POMS[101:120], column.labels = dependents_name[101:120], dep.var.labels.include = FALSE, model.names = TRUE, type = "text", ci = TRUE, ci.level = 0.95, report = "vcstp*", out = "POMS_pre6.txt")
stargazer(models_POMS[121:132], column.labels = dependents_name[121:132], dep.var.labels.include = FALSE, model.names = TRUE, type = "text", ci = TRUE, ci.level = 0.95, report = "vcstp*", out = "POMS_pre7.txt")

