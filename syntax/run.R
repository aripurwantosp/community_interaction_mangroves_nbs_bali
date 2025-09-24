# R file for:
# Run analysis
# 
# For paper:
# 
# Authors of the paper:
# 
# Code by:
# Ari Purwanto Sarwo Prasojo
# 
# Date of this version:
# 2025/09/19


gc()
rm(list=ls())


# library ----
library(tidyverse)
library(haven)
library(janitor)
library(psych)
library(multilevel)
library(olsrr)
library(performance)
library(ggeffects)
library(lmtest)
library(sandwich)
library(logistf)
library(ggstats)
# library(rstatix)
library(broom)
library(broom.helpers)
library(writexl)
library(sdcLog)
library(here)


# working directory path ----
dta_path <- here("data")
syn_path <- here("syntax")
log_path <- here("log")
fig_path <- here("figures")
tbl_path <- here("table")


# functions ----
source(here(syn_path, "utils.R"))


# run ----
## 1. data preparation ----
if(0){
  sdc_log(
    here(syn_path, "1_data_preparation.R"),
    here(log_path, "log_1_data_preparation.txt"),
    replace = TRUE
  )
}

## 2. regression modelling ----
if(0){
  sdc_log(
    here(syn_path, "2_regression_modelling.R"),
    here(log_path, "log_2_regression_modelling.txt"),
    replace = TRUE
  )
}

## 3. supplementary: residuals bootstrap ols asymptotic normal ----
if(0){
  sdc_log(
    here(syn_path, "3_supp_ols_asymptotic.R"),
    here(log_path, "log_3_supp_ols_asymptotic.txt"),
    replace = TRUE
  )
}
