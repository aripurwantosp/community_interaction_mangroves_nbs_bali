# R file for:
# Data preparation
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


# read data ----
df <- readRDS(here(dta_path,"dataset-survei.rds"))
# labelled data
dfl <- df %>% haven::as_factor()
glimpse(dfl)


# Mangrove awareness as a nature-based solution ----
# items:
# h21: mangrove forest contirbutes to reduce the strength of tsunami waves along the coastline
# h22: mangrove forests are capable of mitigating coastal erosion

items <- df %>% dplyr::select(h21:h22)
levels(dfl$h21)
df %>% tabyl(h21)
df %>% tabyl(h22)

# internal consistency ----
#cronbach's alpha
items %>% cronbach

#item total correlation
items %>% item.total

# generate variable ----
# using average score
dfl <- dfl %>% 
  mutate(amasnbs = rowMeans(items))
attr(dfl$amasnbs,"label") <- "Mangrove awareness as a nature-based solution"

dfl$amasnbs %>% mean()
dfl$amasnbs %>% range()

# Vulnerability of living place ----
# items:
# h19: in your opinion, is your residence vulnerable to tsunamis?
# h20: what is the condition of the coastal area near your residence as a result of shoreline erosion?

# reverse scale first
items <- df %>% dplyr::select(h19:h20) %>% 
  mutate_all(~case_when(. == 1 ~ 5,
                        . == 2 ~ 4,
                        . == 3 ~ 3,
                        . == 4 ~ 2,
                        . == 5 ~ 1))
levels(dfl$h19)
df %>% tabyl(h19)
df %>% tabyl(h20)

### internal consistency ----
#cronbach's alpha
items %>% cronbach

#item total correlation
items %>% item.total

# generate variable
# using average score
dfl <- dfl %>% 
  mutate(vuln = rowMeans(items))
attr(dfl$vuln,"label") <- "Vulnerability of living place"

dfl$vuln %>% mean()
dfl$vuln %>% range()


# Sociodemographics ----
# demographics (age,sex,education,occupation,area) ----
dfl %>% tabyl(c02)
dfl %>% tabyl(c04)
dfl %>% tabyl(c03)
dfl %>% tabyl(c05)
dfl %>% tabyl(b02)

dfl <- dfl %>% 
  mutate(age = case_when(c02 %in% c("56-65","65+") ~ "55+",
                         TRUE ~ c02),
         age = factor(age),

         sex = ifelse(c04 == "Laki-laki",1,0),
         sex = factor(sex, levels=0:1, labels = c("Female","Male")),
         
         # recode education into 3 categories
         educ = case_when(c03 %in% c("Tidak Sekolah Formal","Sekolah Dasar") ~ 1,
                          c03 %in% c("SMP","SMA") ~ 2,
                          TRUE ~ 3),
         educ = factor(educ,levels=1:3,labels=c("Primary or less","Secondary","Higher")),
         
         # recode occupation into 4 categories
         occup = case_when(c05 %in% c("Petani","Nelayan","Pedagang") ~ 2,
                           c05 %in% c("Karyawan swasta","ASN/PNS") ~ 3,
                           c05 %in% c("Tidak/belum bekerja","Ibu Rumah Tangga","Pensiunan") ~ 1,
                           TRUE ~ 4),
         # Lainnya -> not employed
         occup = ifelse(!is.na(c05_lainnya) & 
                          c05_lainnya %in% c("Mahasiswa","Irt","Siswa","Kuliah"),
                        1,occup),
         # Lainnya -> trader
         occup = ifelse(!is.na(c05_lainnya) & 
                          c05_lainnya %in% c("Pedagang Ikan","Buruh Tani"),
                        2,occup),
         # Lainnya -> privately/gov/empl
         occup = ifelse(!is.na(c05_lainnya) & 
                          c05_lainnya %in% c("Pekerja Hotel"),
                        3,occup),
         occup = factor(occup,levels=1:4,labels=c("Not employed","Fisherman/farmer/trader",
                                                  "Privately/government employed","Others")),
         area = fct_relabel(b02, ~ifelse(str_detect(.,"Selatan"),
                                         paste("South",str_remove(.," Selatan")),
                                         .))
         )
attr(dfl$age,"label") <- "Age (years)"
attr(dfl$sex,"label") <- "Sex"
attr(dfl$educ,"label") <- "Education"
attr(dfl$occup,"label") <- "Occupation"
attr(dfl$area, "label") <- "Sub-district"

dfl %>% tabyl(age)
dfl %>% tabyl(sex)
dfl %>% tabyl(educ)
dfl %>% tabyl(occup)
dfl %>% tabyl(area)


# Participation in coastal management activities & its driver ----
dfl <- dfl %>% 
  mutate(
    # dependent
    cln_beach = f01_1, #bersih pantai
    plnt_mngrv = f01_2, #menanam mangrove
    cmpg_educ = f01_3, #
    monev = f01_4, #monitoring-evaluasi

    # driver
    inform_plan = df$e05_2, #mengethui program/rencana khusus
    inform_penal = df$e05_4, #mengetahui hukuman bagi pelanggar undang2
    cst_zn = df$e08_6, #zonasi pesisir
    exp_soc = f02_3, #pernah mengikuti pelatihan/sosialisasi prb
  ) %>% 
  # reverse factor levels & relabel/recode
  mutate_at(
    vars(cln_beach,plnt_mngrv,cmpg_educ,monev,exp_soc),
    ~fct_rev(.) %>% fct_recode(.,"No" = "Tidak","Yes" = "Ya")
  )

attr(dfl$cln_beach, "label") <- "Cleaning beach"
attr(dfl$plnt_mngrv, "label") <- "Planting mangroves"
attr(dfl$cmpg_educ, "label") <- "Campaign and education"
attr(dfl$monev, "label") <- "Monitoring and evaluation"

dfl %>% tabyl(cln_beach)
dfl %>% tabyl(plnt_mngrv)
dfl %>% tabyl(cmpg_educ)
dfl %>% tabyl(monev)

attr(dfl$inform_plan, "label") <- "Informed of the coastal management plans"
attr(dfl$inform_penal, "label") <- "Informed penalties for violations"
attr(dfl$cst_zn, "label") <- "Importance of coastal zoning"
attr(dfl$exp_soc, "label") <- "Experiencing in training/socialization related to DRR"

dfl %>% tabyl(inform_plan)
dfl %>% tabyl(inform_penal)
dfl %>% tabyl(cst_zn)
dfl %>% tabyl(exp_soc)


# save file ----
dfl <- dfl %>% 
  dplyr::select(amasnbs:exp_soc,amasnbs_1 = h19, amasnbs_2 = h20, vuln_1 = h19, vuln_2 = h20)
saveRDS(dfl, file = sv_name)


# time end, system info
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))