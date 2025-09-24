# R file for:
# Empirical evidence for asymptotic normal of OLS
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


# initial objects
init_obj <- ls()


# time start, system info
time_start <- Sys.time()
time_start
benchmarkme::get_cpu()
benchmarkme::get_ram()
sessionInfo()


# saved name ----
sv_name <- here(tbl_path, "bootstrap_ols_asymptotic_evidence.xlsx")
sv_fig_name1 <- here(fig_path, "qqplot_ols_asymp_all.png")
sv_fig_name2 <- here(fig_path, "qqplot_ols_asymp_sk.png")
sv_fig_name3 <- here(fig_path, "qqplot_ols_asymp_sd.png")


# read data ----
dfl <- readRDS(here(dta_path,"dataset_for_analysis.rds"))
dfl_sk <- dfl %>% filter(area=="South Kuta")
dfl_sd <- dfl %>% filter(area=="South Denpasar")


# functions ----
# res_boot function in utils.R

# All ----
## fit model ----
mdl_all <- lm(amasnbs ~ age + sex + educ + vuln + occup + area, data = dfl)
coef_all <- coef(mdl_all)
se_classic <- sqrt(diag(vcov(mdl_all)))

## compare classical se and robust se ----
se_rob <- sqrt(diag(vcovHC(mdl_all, type = "HC1")))
se_compare_all <- data.frame(
  term = names(coef_all),
  se_classic = se_classic,
  se_robust = se_rob,
  difference = se_classic - se_rob
)
rownames(se_compare_all) <- NULL
print(se_compare_all)

## bootstrap: distribution for estimator ----
boot_all <- res_boot(mdl_all, B = 1000)
boot_sum_t_all <- data.frame(
  term = names(coef_all),
  mean = apply(boot_all$boot_t, 2, mean),
  sd = apply(boot_all$boot_t, 2, sd),
  skew = apply(boot_all$boot_t, 2, moments::skewness)
)


# South Kuta ----
## fit model ----
mdl_sk <- lm(amasnbs ~ age + sex + educ + vuln + occup, data = dfl_sk)
coef_sk <- coef(mdl_sk)
se_classic <- sqrt(diag(vcov(mdl_sk)))

## compare classical se and robust se ----
se_rob <- sqrt(diag(vcovHC(mdl_sk, type = "HC1")))
se_compare_sk <- data.frame(
  term = names(coef_sk),
  se_classic = se_classic,
  se_robust = se_rob,
  difference = se_classic - se_rob
)
rownames(se_compare_sk) <- NULL
print(se_compare_sk)

## bootstrap: distribution for estimator ----
boot_sk <- res_boot(mdl_sk, B = 1000)
boot_sum_t_sk <- data.frame(
  term = names(coef_sk),
  mean = apply(boot_sk$boot_t, 2, mean),
  sd = apply(boot_sk$boot_t, 2, sd),
  skew = apply(boot_sk$boot_t, 2, moments::skewness)
)


# South Denpasar ----
## fit model ----
mdl_sd <- lm(amasnbs ~ age + sex + educ + vuln + occup, data = dfl_sd)
coef_sd <- coef(mdl_sd)
se_classic <- sqrt(diag(vcov(mdl_sd)))

## compare classical se and robust se ----
se_rob <- sqrt(diag(vcovHC(mdl_sd, type = "HC1")))
se_compare_sd <- data.frame(
  term = names(coef_sd),
  se_classic = se_classic,
  se_robust = se_rob,
  difference = se_classic - se_rob
)
rownames(se_compare_sd) <- NULL
print(se_compare_sd)

## bootstrap: distribution for estimator ----
boot_sd <- res_boot(mdl_sd, B = 1000)
boot_sum_t_sd <- data.frame(
  term = names(coef_sd),
  mean = apply(boot_sd$boot_t, 2, mean),
  sd = apply(boot_sd$boot_t, 2, sd),
  skew = apply(boot_sd$boot_t, 2, moments::skewness)
)


# combine t-stat and make long data frame ----
# variable label
var_label <- tidy_plus_plus(mdl_all)
var_label <- bind_rows(
  data.frame(term = "(Intercept)", var_label = "Intercept", label=""),
  var_label %>% 
  dplyr::select(term,var_label,label)
) %>% 
  mutate(
    id_varlab = 1:n(),
    label = ifelse(term == "vuln","",label),
    varlab = paste0(var_label,":",label),
    varlab = factor(id_varlab,labels=varlab)
  ) %>% 
  dplyr::select(id_varlab, varlab, term)

# long data frame
df_long <- list(
  All = boot_all$boot_t,
  `South Kuta` = boot_sk$boot_t,
  `South Denpasar` = boot_sd$boot_t
) %>% 
  map(.,
    ~{
      mutate(.,id = 1) %>% 
        pivot_longer(-id, names_to = "term", values_to = "t")
    }
  ) %>% 
  bind_rows(.id = "area") %>% 
  dplyr::select(-id) %>% 
  left_join(var_label, by = "term")


# normality formal test using komogorov-smirnov ----
ks_t <- df_long %>% 
  group_by(area,term) %>% 
  group_split(.keep=TRUE) %>% 
  set_names(
    df_long %>% 
      group_by(area,term) %>% 
      group_keys() %>% 
      apply(1, paste, collapse = "_")
  ) %>% 
  map(.,
    ~{
      ks <- ks.test(.$t,"pnorm",mean=0,sd=1)
      data.frame(stat = ks$statistic, pvalue = ks$p.value)
    }
  ) %>% 
  bind_rows(.id = "area_term") %>% 
  separate(col = "area_term", into=c("area","term"),sep = "_") %>% 
  left_join(var_label, by = "term") %>% 
  arrange(area, id_varlab) %>% 
  dplyr::select(area, varlab, stat, pvalue)

# save output to excel ----
writexl::write_xlsx(
  x = list(
    se_all = se_compare_all,
    boot_all = boot_sum_t_all,
    se_sk = se_compare_sk,
    boot_sk = boot_sum_t_sk,
    se_sd = se_compare_sd,
    boot_sd = boot_sum_t_sd,
    ks_test_t = ks_t
  ),
  path = sv_name
)

# QQ plot ----
# all
df_long %>% filter(area == "All") %>% 
  ggplot(aes(sample=t)) +
  stat_qq(distribution = qnorm) + stat_qq_line(distribution = qnorm) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  facet_wrap(varlab ~ ., ncol = 3) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )
ggsave(sv_fig_name1, width = 8, height = 7.5, units = "in", dpi = 300)

# south kuta
df_long %>% filter(area == "South Kuta") %>% 
  ggplot(aes(sample=t)) +
  stat_qq(distribution = qnorm) + stat_qq_line(distribution = qnorm) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  facet_wrap(varlab ~ ., ncol = 3) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )
ggsave(sv_fig_name2, width = 8, height = 6, units = "in", dpi = 300)

# south denpasar
df_long %>% filter(area == "South Denpasar") %>% 
  ggplot(aes(sample=t)) +
  stat_qq(distribution = qnorm) + stat_qq_line(distribution = qnorm) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  facet_wrap(varlab ~ ., ncol = 3) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )
ggsave(sv_fig_name3, width = 8, height = 6, units = "in", dpi = 300)



# df_plt %>% filter(area == "All") %>% 
#   ggplot(aes(x = t)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 30, color="white") +
#   # geom_density() +
#   stat_function(fun = dnorm, args = list(mean = 0, sd = 1), linetype="dashed") +
#   facet_wrap(varlab ~ .) +
#   theme_bw() +
#   theme(
#     panel.grid = element_blank()
#   )


# time end, system info
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))