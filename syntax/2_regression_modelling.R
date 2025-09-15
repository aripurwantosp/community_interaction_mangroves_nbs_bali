# R file for:
# Regression modelling
# 
# For paper:
# 
# Authors of the paper:
# 
# Code by:
# Ari Purwanto Sarwo Prasojo
# 
# Date of this version:
# 2025/09/14


# initial objects
init_obj <- ls()


# time start, system info
time_start <- Sys.time()
time_start
benchmarkme::get_cpu()
benchmarkme::get_ram()
sessionInfo()


# saved name ----
sv_name <- here(dta_path, "dataset_for_analysis.rds")
sv_fig_name1 <- here(fig_path, "mangrove_awareness_mean_prediction.png")
sv_fig_name2 <- here(fig_path, "participation_coef.png")


# read data ----
dfl <- readRDS(here(dta_path,"dataset_for_analysis.rds"))
dfl_sk <- dfl %>% filter(area=="South Kuta")
dfl_sd <- dfl %>% filter(area=="South Denpasar")


# OLS: Mangrove awareness as a nature-based solution ----
## all ----
# fit model
mdl_ols <- lm(amasnbs ~ age + sex + educ + vuln + occup + area, data = dfl)
# check assumption
check_collinearity(mdl_ols)
ols_test_normality(mdl_ols)
ols_test_breusch_pagan(mdl_ols)
# model summary with robust se
coeftest(mdl_ols, vcov = vcovHC(mdl_ols, "HC0"))
waldtest(
  update(mdl_ols, . ~ 1), mdl_ols,
  vcov = vcovHC(mdl_ols, type = "HC0"), test = "F"
)
model_performance(mdl_ols)

## south kuta ----
# fit model
mdl_ols_sk <- lm(
  amasnbs ~ age + sex + educ + vuln + occup, 
  data = dfl %>% filter(area=="South Kuta")
)
# check assumption
check_collinearity(mdl_ols_sk)
ols_test_normality(mdl_ols_sk)
ols_test_breusch_pagan(mdl_ols_sk)
# model summary with robust se
coeftest(mdl_ols_sk, vcov = vcovHC(mdl_ols_sk, "HC0"))
waldtest(
  update(mdl_ols_sk, . ~ 1), mdl_ols_sk,
  vcov = vcovHC(mdl_ols_sk, type = "HC0"), test = "F"
)
model_performance(mdl_ols_sk)

## south denpasar ----
mdl_ols_sd <- lm(
  amasnbs ~ age + sex + educ + vuln + occup, 
  data = dfl %>% filter(area=="South Denpasar")
)
# check assumption
check_collinearity(mdl_ols_sd)
ols_test_normality(mdl_ols_sd)
ols_test_breusch_pagan(mdl_ols_sd)
# model summary with robust se
lmtest::coeftest(mdl_ols_sd, vcov = vcovHC(mdl_ols_sd, "HC0"))
waldtest(
  update(mdl_ols_sd, . ~ 1), mdl_ols_sd,
  vcov = vcovHC(mdl_ols_sd, type = "HC0"), test = "F"
)
model_performance(mdl_ols_sd)

# forest plot predicted means
ggcoef_compare(
  list("All" = mdl_ols,
       "South Kuta" = mdl_ols_sk,
       "South Denpasar" = mdl_ols_sd
  ),
  tidy_fun = tidy_all_effects,
  tidy_args = list(
    xlevels=list(vuln=c(1.5, 3, 4.5)),
    # robust se
    vcov = vcovHC,
    type = "HC0"
  ),
  type = "faceted",
  vline = FALSE,
  point_size = 1,
  point_stroke = 1,
  errorbar_height = 0,
  facet_labeller = ggplot2::label_wrap_gen(25)
) +
  # labs(title="Awareness of mangrove as nature based solution") +
  # ggplot2::scale_y_discrete(labels = scales::label_wrap(15)) +
  theme(axis.text = element_text(size=10,color="black"))

ggsave(sv_fig_name1, width = 8.3, height = 10, units = "in", dpi = 300)


# Logit: Participation in coastal management activities ----

## utils functions ----
# get variable label
get_label <- function(x){attr(dfl[[x]],"label")}

# fun to fit model
# notes:
# form: vector of formula
# data frame and formula (string) as input
logit_fit <- function(form, df) {
  est <- form %>%
    # map(., ~glm(formula(.), family = binomial(), data = df))
    map(., ~logistf(formula(.), family = binomial(), data = df))
  return(est)
}

# fun to estimate (coefficient, or)
# model list and data frame as input
logit_tidy <- function(mdl_list, conf.level = 0.95, exponentiate=TRUE, df) {
  est <- mdl_list %>% 
    map(., ~tidy_plus_plus(., conf.int=TRUE,conf.level=conf.level,exponentiate = exponentiate)) %>% 
    map(., ~filter(., var_type == "dichotomous" & reference_row == FALSE | var_type == "continuous")) %>% 
    map(., ~slice(., 1)) %>%
    map(.,~dplyr::select(.,variable,estimate,conf.low,conf.high)) %>% 
    bind_rows(.id = "formula") %>% 
    mutate(
      dep = pairs$Var1,
      indep = pairs$Var2,
      dep_label = sapply(dep, get_label, USE.NAMES = FALSE),
      indep_label = sapply(indep, get_label, USE.NAMES = FALSE)
    ) %>% 
    relocate(dep:indep_label,.before="formula")
  return(est)
}

## all ----
# combination of formula/term
indep_list <- c("inform_plan","inform_penal","cst_zn","exp_soc")
dep_list <- c("cln_beach","plnt_mngrv","cmpg_educ","monev")
sociodem_all <- " + age + sex + educ + vuln + occup"
pairs <- expand.grid(dep_list, indep_list, stringsAsFactors = FALSE) %>% 
  mutate(mdl_form = paste(Var1, "~", Var2, sociodem_all))

# fit model
mdl_all <- logit_fit(pairs$mdl_form, df=dfl) %>% 
  set_names(pairs$mdl_form)

# check vif
map(mdl_all, ~performance::check_collinearity(.)) %>% 
  set_names(pairs$mdl_form)

map(mdl_all, 
  ~{
    performance::check_collinearity(.) %>% 
      as.data.frame() %>% 
      pull(VIF) %>% 
      mean(.)
  }
  ) %>% 
  set_names(pairs$mdl_form)

# summary
for(mdl in mdl_all){
  print(summary(mdl))
}

# extract coef
coef_all <- logit_tidy(mdl_all, exponentiate = FALSE)

## south kuta ----
sociodem_sub <- " + age + sex + educ + vuln + occup"
pairs <- expand.grid(dep_list, indep_list, stringsAsFactors = FALSE) %>% 
  mutate(mdl_form = paste(Var1, "~", Var2, sociodem_sub))

# fit model
mdl_sk <- logit_fit(pairs$mdl_form, df=dfl_sk) %>% 
  set_names(pairs$mdl_form)

# summary
for(mdl in mdl_sk){
  print(summary(mdl))
}

# check vif
map(mdl_sk, ~performance::check_collinearity(.)) %>% 
  set_names(pairs$mdl_form)

map(mdl_sk, 
  ~{
    performance::check_collinearity(.) %>% 
      as.data.frame() %>% 
      pull(VIF) %>% 
      mean(.)
  }
  ) %>% 
  set_names(pairs$mdl_form)

# extract or
coef_sk <- logit_tidy(mdl_sk, exponentiate = FALSE)

## south denpasar ----
# fit model
mdl_sd <- logit_fit(pairs$mdl_form, df=dfl_sd) %>% 
  set_names(pairs$mdl_form)

# summary
for(mdl in mdl_sd){
  print(summary(mdl))
}

# check vif
map(mdl_sd, ~performance::check_collinearity(.)) %>% 
  set_names(pairs$mdl_form)

map(mdl_sd, 
  ~{
    performance::check_collinearity(.) %>% 
      as.data.frame() %>% 
      pull(VIF) %>% 
      mean(.)
  }
  ) %>% 
  set_names(pairs$mdl_form)

# extract or
coef_sd <- logit_tidy(mdl_sd, exponentiate = FALSE) 

# make coef plot ----
# data for plot
df_plt <- rbind(
  coef_all %>% mutate(area = "All"),
  coef_sk %>% mutate(area = "South Kuta"),
  coef_sd %>% mutate(area = "South Denpasar")
) %>% 
  mutate(area = factor(area,levels=c("All","South Kuta","South Denpasar")),
         dep_label = factor(dep_label,levels=unique(.$dep_label)),
         indep_label = factor(indep_label,levels=unique(.$indep_label))
  )

# plot
df_plt %>% 
  # mutate(sig = ifelse(p.value > 0.05, 1,0),
  #        sig = factor(sig, levels=c(0,1), labels =c("p<=0.05","p>0.05"))) %>% 
  ggplot(aes(y=dep_label,x=estimate,color=indep_label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size=1.5,position=position_dodge(width=0.5)) +
  geom_errorbar(aes(y=dep_label,xmin=conf.low,xmax=conf.high),
                position=position_dodge(width=0.5),
                linewidth = .5, width=0) +
  labs(x = "Coefficient", y="Dependent",color="Independent") +
  # scale_shape_manual(values=c("p<=0.05" = 16,"p>0.05" = 8)) +
  # ggthemes::scale_color_tableau() +
  facet_grid(area~.) +
  guides(colour = guide_legend(nrow = 2)) +
  theme_bw() +
  theme(
    axis.text = element_text(size=10, color="black"),
    strip.text = element_text(size= 10, color = "black"),
    legend.position = "bottom"
  )

ggsave(sv_fig_name2, width = 8.3, height = 9, units = "in", dpi = 300)


# time end, system info
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))