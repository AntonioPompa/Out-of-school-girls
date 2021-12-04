###########################
#
# Final Project - STATS 
# 
# ------------------
# Objective: Predict Out-of-School Girls in India

# Author: Antonio Pompa Rangel
# Co-authors: Kurt Tsuo, Pranav Bhargava, Rita Rudnik
# Initial date: 1-Nov-2021
# Date last modified: 1-Dec-2021
      # - Version control: includes bootstrapping exercise for Udaipur
# Contact: antoniopompa@hks.harvard.edu
###########################

# the usual stuff
setwd("/Users/antoniopomparangel/Desktop/MPA ID Harvard/00 Coursework/01 First semester/API 209 Stats/FINAL group exercise/Option 1 - Educate Girls India/")
rm(list = ls())

library(tidyverse)
library(dplyr)
library(pmdplyr)
library(ggplot2)
library(ggpubr)
library(readxl)
library(ggthemes)
library(scales)
library(stargazer)
library(corrgram)
  library(corrplot)
library(naniar)

#libraries related to ML -  Might to to run the install.packages
library(caret)
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(foba)
library(monomvn)
library(rsample)
library(boot) # used for bootstrapping function

# There is a conflict between a library and dplyr so:
select <- dplyr::select
# Load csv

aser <- read_csv("data/aser_data.csv")
d2d <- read_csv("data/d2d_data.csv")
census <- read_csv("data/dise_census_training.csv")

#===============
# 1. Pre-Processing
#=============== 

# DISE
# Keep most recent value of DISE in Census Data

test <- census %>%      #Just a test to see how to do it in loop later
  select(ends_with("1314"), ends_with("1617"), ends_with("1718")) %>% 
  select(starts_with("numschools")) %>% 
  mutate(numschools = ifelse(is.na(numschools1718) == F, numschools1718,
                             ifelse(is.na(numschools1617) == F, numschools1617,
                                    numschools1314)))
head(test, n = 10)

#Setting up vector of DISE variables
vars <- c("apprb5",
          "apprg5",
          "approachbyroad",
          #"blackboard",
          "bld_govt",
          "bld_incomp",
          "bld_nobld",
          "bld_private",
          #"bldstatus",
          #"block_name_dise",
          "bndrywall_none",
          "bndrywall_other",
          "bndrywall_partial",
          "bndrywall_pucca",
          "bookinlib",
          #"boy_girl_ratio",
          "cal",
          "cce",
          "clgood",
          "clnotgood",
          "clrooms",
          "computer",
          "dis_b",
          "dis_g",
          #"district_name_dise",
          "electric",
          "estdyear",
          "funds_received",
          #"gen_b",
          #"gen_g",
          "gradabove",
          "grade1_b",
          "grade1_g",
          "grade2_b",
          "grade2_g",
          "grade3_b",
          "grade3_g",
          "grade4_b",
          "grade4_g",
          "grade5_b",
          "grade5_g",
          "grade6_b",
          "grade6_g",
          "grade7_b",
          "grade7_g",
          "grade8_b",
          "grade8_g",
          "headtch",
          "higher_secondary",
          "hmroom",
          "kitshed_avail",
          "library",
          #"main_cl_mc",
          "medchk",
          "medinstr_english",
          "medinstr_hindi",
          "medinstr_other",
          "medinstr_urdu",
          "midday_meals",
          "numschools",
          "obc_b",
          "obc_g",
          "p60b5",
          "p60g5",
          "passb5",
          "passg5",
          "pcr_maintained",
          "pcr_shared",
          "playground",
          "ppsec",
          "primary",
          "primary_only",
          "ramps",
          "rep_b",
          "rep_g",
          "sc_b",
          "sc_g",
          "schhrstch",
          "school_deped",
          "school_local",
          "school_madarsa",
          "school_other",
          "school_private",
          "school_rural",
          "schooldays",
          #"schooldays_missing",
          "schres",
          "secondary",
          "smcmeetings",
          "smcmem_parent",
          "smcmem_tot",
          "smcsdp",
          "smschildren",
          "st_b",
          "st_g",
          "student_teacher_ratio",
          "tch_female",
         # "tch_male",
          "tch_total",
          "tchwithprof",
          "toilet_b",
          "toilet_g",
          "tot_b",
          "tot_g",
          "txtbkrecd",
          "upper_primary",
          #"urban",
         # "village_[year]",
          #"village_code_dise",
          #"village_name_dise",
          "visitsbrc",
          "visitscrc",
          "water_hndpmp",
          "water_none",
          "water_other",
          "water_tap",
          "water_well",
          "wsec25p_any"
)

length(vars)

#the loop for keeping latest value available
for(var_i in vars) {       
  print(var_i)
  var1718 <- paste(var_i, "1718", sep = "")
  var1617 <- paste(var_i, "1617", sep = "")
  var1314 <- paste(var_i, "1314", sep = "")
  census <- census %>% 
    mutate( !!sym(var_i) := ifelse(is.na(!!sym(var1718)) == F, !!sym(var1718),
                                   ifelse(is.na(!!sym(var1617)) == F, !!sym(var1617),
                                          !!sym(var1314))
    ))
}

test2 <- census %>% 
  select(starts_with("numschool"))

head(test2)

# Filter data to keep variables created above, remove data with years
census <- census %>% 
  select(-ends_with("1314")) %>% 
  select(-ends_with("1617")) %>%
  select(-ends_with("1718"))

# small ggplot for pset9
ggplot(data = d2d, aes(x = District_D2D, y = oos_g_5to14)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "District",
       y = "Number of out-of-school girls by village",
       title =  "Distribution of out-of-school girls by District")


# ==========
# 2. Merge between data
# ==========

#hist(d2d$oos_g_5to14)

dtrain <- d2d %>% 
  left_join(census, by = "VillageCode" )

# ==========
# 3. Analysis of missing values
# ==========

na_count <-sapply(dtrain, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

na_graph <- select(dtrain, numschools, roof_material_2c11, tot_pc11, households_w_latrine_5c11)
naniar::vis_miss(na_graph, warn_large_data = FALSE)

prop.table(table(dtrain$missing_census))
prop.table(table(dtrain$missing_h_census))

gr_miss_dise_vs_d2d <- ggplot(data = dtrain, aes(x = numschools, y = oos_g_5to14)) +
  geom_miss_point() +
  labs(x = "Number of schools 16/17 (DISE data)",
       y = "Number of out-of-school girls \nby village (D2D data)",
       title =  "Distribution of missing values",
       subtitle = "DISE vs D2D")
gr_miss_dise_vs_d2d

# do another graph for the distribution of missing values

gr_miss_dise_vs_census <- ggplot(data = dtrain, aes(x = f_06c11, y = oos_g_5to14)) +
geom_miss_point() +
  labs(x = "Number of females age 0 to 6 years (Census data)",
       y = "Number of out-of-school girls \nby village (D2D data)",
       title =  "Distribution of missing values",
       subtitle = "Census vs D2D")
gr_miss_dise_vs_census

# do another graph fr

ggsave(file = "output/gr_miss_dise_vs_d2d.jpeg",
       plot = gr_miss_dise_vs_d2d)
ggsave(file = "output/gr_miss_dise_vs_census.jpeg",
       plot = gr_miss_dise_vs_census)

gr_missing <- dtrain %>% 
  filter(missing_census == 1) %>% 
  select(oos_g_5to14)
gr_n_missing <- dtrain %>% 
  filter(missing_census == 0) %>% 
  select(oos_g_5to14)


dtrain <- dtrain %>% 
  mutate(miss_cen_chr = ifelse(missing_census==1, "Missing", "Not Missing"))

ggplot(data = dtrain, aes(x = log(oos_g_5to14), fill = miss_cen_chr)) +
  geom_histogram(alpha = 0.3, bins = 15) + 
  labs(title = "Distribution of OOSGs, missing vs \nnon-missing values in Census",
       x = "Logarithm of OOSGs",
       y = "Proportion",
       fill = "Missing values") +
  theme(legend.position="right")


mcar_test(select(dtrain, numschools, oos_g_5to14))

missing_list <- list("missing_census", "missing_h_census", "missing_p_census")
mar_test <- glm(missing_census ~ oos_g_5to14, dtrain, family = "binomial")
summary(mar_test)
exp(coef(mar_test))

summary(glm(missing_h_census ~ oos_g_5to14, dtrain, family = "binomial"))

# ======
# 4. Creating new variables for train
#   - I transform some variables that might give a better outlook than 
#    just adding the variables by themselves
#   - Feel free to add and modify however you want
# ======
dtrain <- dtrain %>%
  mutate(smc_par_ratio = smcmem_parent/smcmem_tot, #can be zero if no parent on smc
         toil_gen_prop = toilet_g/(toilet_g + toilet_b), #prop of girls toilet to boys
         tot_child = ifelse( tot_b + tot_g == 0, NA, tot_b + tot_g), #total child
         girl_prop = tot_g/tot_child,               #  Proportion of girls       
         num_book_per_child = (bookinlib*numschools)/tot_child, # num books per total child
         clnotgood_prop = clnotgood/clrooms,
         num_comp_per_child = (computer*numschools)/tot_child 
  )       
  
# Correlations      

d_gr_corr <- select( dtrain, 
                    oos_g_5to14, smc_par_ratio, toil_gen_prop, 
                    tot_b, tot_g, tot_child, num_book_per_child, 
                    f_06c11, f_illc11, f_litc11, main_hh_fc11, 
                    mainwork_fc11, no_hhc11, tot_fc11, numschools)
M <- cor(d_gr_corr, use = "pairwise.complete.obs")
M

# Correlation 1 graph
jpeg(file="output/corr1.jpeg")
corr1 <- corrplot(M, method = "ellipse", order = "hclust")
corr1
dev.off()

jpeg(file="output/corr2.jpeg")
corr2 <- corrgram(d_gr_corr, 
         order = TRUE, 
         lower.panel=panel.pts, 
         upper.panel=panel.ellipse, 
         text.panel=panel.txt, 
         main="Correlation between \nsome relevant variables")
dev.off()

# ======
# 5. We start with some MLearning!!!
# ======

#   first, we set the seed and the K-Folding (you can play with the parameters)
set.seed(3472)
cvParameters <- trainControl(method = "cv", number = 10, summaryFunction=defaultSummary)

str(cvParameters)

# Second, setup of various formulas to be used on the estimation
# feel free to play with this
formula_short <- as.formula(oos_g_5to14 ~ smc_par_ratio + toil_gen_prop + 
                          tot_b + tot_g + tot_child + num_book_per_child +  
                          f_06c11 + f_illc11 + f_litc11 + main_hh_fc11 + 
                          mainwork_fc11 + no_hhc11 + tot_fc11 + numschools)

formula_kurt <- as.formula(oos_g_5to14 ~ 
                           f_litc11 +
                           f_scc11 +
                           f_stc11 +
                           house_condition_residence_3c11 +
                           house_condition_residence2_4c11 +
                          house_structure_2c11 +       #YOU CAN JUST CTRL + SHIFT + C ALL THESE LINES
                          house_structure_3c11 +
                          house_structure_5c11 +
                          main_al_fc11 +
                          main_hh_fc11 +
                          main_ot_fc11 +
                          mainwork_fc11 +
                          marg_al_0_3_fc11 +
                          # marg_al_3_6_fc11 +
                          # marg_cl_0_3_fc11 +
                          # marg_cl_3_6_fc11 +
                          # marg_hh_0_3_fc11 +
                          # marg_hh_3_6_fc11 +
                          # marg_ot_0_3_fc11 +
                          # marg_ot_3_6_fc11 +
                          # margwork_0_3_fc11 +
                          # margwork_3_6_fc11 +
                          # non_work_fc11 +
                          # source_drinking_water_2c11 +
                          # source_lighting_6c11 +
                          # tot_fc11 +
                          # tot_work_fc11 +
                          # apprg5 +   #  DISE data starts here
                          # approachbyroad +
                          # bld_govt +
                          # bld_incomp +
                          # bld_nobld +
                          # bld_private +
                          # bndrywall_none +
                          # bndrywall_other +
                          # bndrywall_partial +
                          # bndrywall_pucca +
                          # bookinlib +
                          # cce +
                          # clgood +
                          # clnotgood +
                          # clrooms +
                          # computer +
                          # dis_g +
                          #  electric +
                          #  funds_received +
                          #  gradabove +
                          #  grade1_b - grade1_g +
                          #  grade2_b - grade2_g +
                          #  grade3_b - grade3_g +
                          #  grade4_b - grade4_g +
                          #  grade5_b - grade5_g +
                          #  grade6_b - grade6_g +
                           grade7_b - grade7_g +
                           grade8_b - grade8_g +
                           headtch +
                           higher_secondary +
                           kitshed_avail +
                           library +
                           medchk +
                           medinstr_hindi +
                           medinstr_english +
                           medinstr_other +
                           medinstr_urdu +
                           midday_meals +
                          # main_cl_mc +
                           numschools +
                           obc_b +
                           obc_g +
                           obc_b - obc_g +
                           p60b5 +
                           p60g5 +
                           p60b5 - p60g5 +
                           passb5 +
                           passg5 +
                           passb5 - passg5 +
                           pcr_maintained +
                           pcr_shared +
                           playground +
                           ppsec +
                           primary +
                           primary_only +
                           ramps +
                           rep_b +
                           rep_g +
                           rep_b - rep_g +
                           sc_b +
                           sc_g +
                           sc_b - sc_g +
                           sc_b / tot_b +
                           sc_g / tot_g +
                           schhrstch +
                           school_deped +
                           school_local +
                           school_madarsa +
                           school_other +
                           school_private +
                           school_rural +
                           schooldays +
                           schres +
                           secondary + 
                           smcmeetings + 
                           smcmem_parent + 
                           smcmem_tot + 
                           smcsdp + 
                           smschildren + 
                           st_b + 
                           st_g + 
                           student_teacher_ratio + 
                           tch_female + 
                       #    tch_male + 
                           tch_total + 
                           tchwithprof + 
                           toilet_b + 
                           toilet_g + 
                           tot_b + 
                           tot_g + 
                          # tot_b - tot_g  Had to comment this be cause of perfect colinearity
                           txtbkrecd + 
                           upper_primary + 
                           visitsbrc + 
                           visitscrc + 
                           water_hndpmp + 
                           water_none + 
                           water_other + 
                           water_tap + 
                           water_well + 
                           wsec25p_any)
  

# DEFINE WHICH FORMULA YOU WANT TO USE IN NEXT SECTION

# whatever formula you built, replace it here and ML models will feed on this
formula_master <- formula_short

# Linear model
modelLin <- train(formula_master, 
                  data = dtrain,
                  method="lm",
                  metric="RMSE",
                  na.action = na.omit,
                  trControl = cvParameters)
summary(modelLin)
modelLin
dtrain$predtest <- predict(modelLin, newdata =  dtrain, na.action = na.pass)

# Lasso regression
modelLasso <- train(formula_master,
                    data = dtrain,
                    method="lasso",
                    metric="RMSE",
                    na.action = na.omit,
                    trControl = cvParameters)
summary(modelLasso)
modelLasso
modelLasso$finalModel


# Random Forest - (takes a LOOOOOONG TIME TO RUN)
# modelRF <- train(formula_master,
#                     data = dtrain,
#                     method="rf",
#                     metric="RMSE",
#                     na.action = na.omit,
#                     trControl = cvParameters)
# summary(modelRF)

# Random Partition
modelRP <- train(formula_master,
                  data = dtrain,
                  method="rpart",
                  metric="RMSE",
                  na.action = na.omit,
                  trControl = cvParameters)
summary(modelRP)

# Ridge Regression
modelRidge <- train(formula_master,
                   data = dtrain,
                   method = "ridge",
                   metric="RMSE",
                   na.action = na.omit,
                   trControl = cvParameters)
summary(modelRidge)
modelRidge

# Ridge elastic
modelElasticN <- train(formula_master,
                    data = dtrain,
                    method = "enet",
                    metric="RMSE",
                    na.action = na.omit,
                    trControl = cvParameters)
summary(modelElasticN)

# Gam Loess model
modelGamLoess <- train(formula_master,
                    data = dtrain,
                    method = "gamLoess",
                    metric="RMSE",
                    na.action = na.omit,
                    trControl = cvParameters)
summary(modelGamLoess)

# Bayesian Ridge model - ALSO TAKES A LOOOOONG TIME
# modelBRidge <- train(formula_master,
#                        data = dtrain,
#                        method = "bridge",
#                        metric="RMSE",
#                        na.action = na.omit,
#                        trControl = cvParameters)
# summary(modelBRidge)

# K-nearest neighbor
modelKnearest <- train(formula_kurt,
                       data = dtrain,
                       method = "kknn",
                       metric="RMSE",
                       na.action = na.omit,
                       trControl = cvParameters)
summary(modelKnearest)

modelLin
modelLasso
#modelRF
modelRP
modelRidge
modelElasticN
modelGamLoess
modelBRidge
modelKnearest

#==========
# Finally, we predict using these models for each district:
#==========

# We NEED a very parsimonous model. In 4 districts we predict less than 100 villages, very few!!!
table(dtrain$District.x)
district_name <- sort(unique(dtrain$District.x))
district_name

for (name in district_name) {       
  #fit models here
  loop_train <- filter(dtrain, District.x != paste(name))
    modelL_fin <- lm(formula_master, loop_train, na.action = na.omit)
    modelLasso_fin <- train(formula_short,
                        data = loop_train,
                        method="lasso",
                        metric="MSE",
                        na.action = na.omit,
                        trControl = cvParameters)
  #predict values here
  loop_holdout <- filter(dtrain, District.x == paste(name))
    loop_holdout$pred1 <- predict(modelL_fin, loop_holdout)
    mse1 <- hydroGOF::mse(loop_holdout$pred1, loop_holdout$oos_g_5to14)
    
    loop_holdout$pred2 <- predict(modelLasso_fin, loop_holdout, na.action = na.pass)
    mse2 <- hydroGOF::mse(loop_holdout$pred2, loop_holdout$oos_g_5to14)
    
    print(paste(name, rmse1, rmse2, sep = " ---- "))
    #print(paste(name, sum(is.na(loop_holdout$pred1)))) # to check how many NAs
    
}

#Just to check. At the end, District UDAIPUR should be on the test and out of train. So it works!
table(loop_train$District.x)
table(loop_holdout$District.x)


#==========
# Predicting OOSGs for UDAIPUR:
#==========

dpredict <- d2d %>% 
  right_join(census, by = "VillageCode" ) %>% 
  filter(District.y == "UDAIPUR") %>% 
  mutate(smc_par_ratio = smcmem_parent/smcmem_tot, #can be zero if no parent on smc
         toil_gen_prop = toilet_g/(toilet_g + toilet_b), #prop of girls toilet to boys
         tot_child = ifelse( tot_b + tot_g == 0, NA, tot_b + tot_g), #total child
         girl_prop = tot_g/tot_child,               #  Proportion of girls       
         num_book_per_child = (bookinlib*numschools)/tot_child, # num books per total child
         clnotgood_prop = clnotgood/clrooms,
         num_comp_per_child = (computer*numschools)/tot_child 
  )     


dpredict$pred1 <- predict(modelL_fin, dpredict, interval = c("confidence"))
dpredict$pred2 <- predict(modelLasso_fin, dpredict, na.action = na.pass, interval = c("confidence"))

# calculate quantiles of predictions
quantile(predict(modelLasso_fin, dpredict, s = "lambda.min"))

# # Save predicted values down with site ids
d_bootstrap <- tibble(
  "site_id" = dpredict$VillageCode,
  "site_name" = dpredict$Village.y,
  "y_actual" = dpredict$oos_g_5to14,
  "y_pred_lasso" = predict(modelLasso_fin, na.action = na.pass, dpredict, s = "lambda.min")
)

# Create percentile category variables for the actual and predicted values

d_bootstrap <- d_bootstrap %>%
  mutate(
    y_actual_percentile = ntile(y_actual, n = 10),
    y_pred_percentile = ntile(y_pred_lasso, n = 10),
  )

predicted <- d_bootstrap

# Specify number of bootstrap iterations to run
n_bootstrap <- 1000

for (i in 1:n_bootstrap) {
  
  # Sample with replacement to determine which rows are in my new data sample
  samp_ids <- sample(x = nrow(dpredict), size = nrow(dpredict), replace = T)
  # Extract the rows that were sampled for this iteration
  train_loop <- dpredict[samp_ids,]
  # Fit the LASSO model to simulated sample
  modelLasso_fin <- train(formula_short,
                          data = train_loop,
                          method="lasso",
                          metric="RMSE",
                          na.action = na.omit,
                          trControl = cvParameters)
  
  # Save down prediction measures with site_ids and actual values
  d_loop <- tibble(
    "site_id" = dpredict$VillageCode,
    "site_name" = dpredict$Village.y,
    "y_actual" = dpredict$oos_g_5to14,
    "y_pred_lasso" = predict(modelLasso_fin, na.action = na.pass, dpredict)
  )
  
  # Create percentile assignments within this bootstrapped sample
  d_loop <- d_loop %>% 
    mutate(
      y_actual_percentile = ntile(y_actual, n = 10),
      y_pred_percentile = ntile(y_pred_lasso, n = 10),
    )
  
  # Add this simulated sample to my previously saved data
  d_bootstrap <- bind_rows(d_bootstrap, d_loop)
  
}

head(d_bootstrap)
dim(d_bootstrap)

dim(dpredict)

# Look briefly at a random selection of sites and the bootstrap results
ci_bootstrap <- d_bootstrap %>% 
  #filter(site_id %in% sample(dpredict$VillageCode, size = 2692, replace = F)) %>% 
  # group calculations by site id
  group_by(site_id, site_name) %>% 
  # calculate summary statistics
  summarise(
    mean_y_pred = mean(y_pred_lasso),
    ci_y_lower = quantile(y_pred_lasso, c(0.05), na.rm = TRUE),
    ci_upper = quantile(y_pred_lasso, c(0.95), na.rm = TRUE),
    var_y_pred = var(y_pred_lasso),
    mean_y_pred_decile = mean(y_pred_percentile)
  ) %>% 
  arrange(desc(mean_y_pred))

results_finals <- left_join(predicted, ci_bootstrap, by = "site_id")

results_finals_na <- filter(results_finals, is.na(y_pred_lasso))

udaibur_ba <- dpredict %>% 
  filter(missing_census == 1)

write.csv(results_finals, file = "output/predicted_with_ci.csv")
