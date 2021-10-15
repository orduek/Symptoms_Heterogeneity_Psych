##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 2.0 (01.06.2021)                        #
#                                                                            #
#----------------------------------------------------------------------------#
#                                                                            #
#                        Script III - SUBROUP Analyses                       #
#									                                                           #
#----------------------------------------------------------------------------#
#                                                                            #
#                                DATA:  PHQ9                                 #
#									                                                           #
##############################################################################


###### Table of Contents #####################################################
#----- 1. Load libraries ----------------------------------------------------#
#----- 2. Import and prepare data -------------------------------------------#
#----- 3. Descriptive -------------------------------------------------------#
#----- 4. Create subgroups --------------------------------------------------#
#----- 5. Assessment of symptom phenotypes ----------------------------- ----#
#-----  5.1 Low  ------------------------------------------------------------#
#-----   5.1.1 Phenotypes  --------------------------------------------------#
#-----   5.1.2 Jaccard-Index ------------------------------------------------#
#-----   5.1.3 Plot the phenotypes distribution -----------------------------#
#-----  5.2 Medium ----------------------------------------------------------#
#-----   5.2.1 Phenotypes  --------------------------------------------------#
#-----   5.2.2 Jaccard-Index ------------------------------------------------#
#-----   5.2.3 Plot the phenotypes distribution -----------------------------#
#-----  5.3 High ------------------------------------------------------------#
#-----   5.3.1 Phenotypes  --------------------------------------------------#
#-----   5.3.2 Jaccard-Index ------------------------------------------------#
#-----   5.3.3 Plot the phenotypes distribution -----------------------------#
#----- 6. Test distributions ------------------------------------------------#
#-----  6.1 Low  ------------------------------------------------------------#
#-----  6.2 Medium ----------------------------------------------------------#
#-----  6.3 High ------------------------------------------------------------#
#----- 7. Session Info  -----------------------------------------------------#

## General Notes ##
#In PHQ9 we use non binarized data
# data2 = binarized dataframe
# datax = dataframe script one with scores, binarized items and frequency profiles

###### 1. Load Libraries #####################################################
# Data handling + basic calculations
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("foreign")) install.packages("foreign")
if(!require("ReIns")) install.packages("ReIns")

#Power Law
if(!require("poweRlaw")) install.packages("poweRlaw")

# Add theme_minimal_hgrid
if(!require("cowplot")) install.packages("cowplot")


###### 2. Import and prepare data ############################################
## Prepare
# Load datax & data2 created in script one
datax <- read_delim("Analysis/PHQ9/Generated Data/Matched_freq_count.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

###### 3. Descriptive #######################################################
## Datax
summary(datax)
nrow(datax)  # 165397

# Summed severity
hist(datax$total)
mean(datax$total) # 13.38
sd(datax$total) # 6.83

# Summed severity of non-binarized items
hist(datax$total_bin)
summary(datax$total_bin) # Q1 = 2, Q3 = 7, Median = 5

###### 4. Create subgroups ###################################################
summary(datax$total) # Q1,Q3 = 8, 19, Median = 14

#####Low
data_sub_low <- datax %>%  #Q1
  filter(total == 8)

# Create new data frame
data2_low <- data_sub_low %>% 
  select(q1:q9) %>% 
  
  tibble()

## Count frequency of profiles
data2_low_counted <- plyr::count(data2_low[, ])

##### Medium
data_sub_med <- datax %>%  #Median
  filter(total == 14)

# Create new data frame
data2_med <- data_sub_med %>% 
  select(q1:q9) %>% 
  tibble()

## Count frequency of profiles
data2_med_counted <- plyr::count(data2_med[, ])

##### High
data_sub_high <- datax %>%  #Median
  filter(total == 19) #Q3

# Create new data frame
data2_high <- data_sub_high %>% 
  select(q1:q9) %>% 
  tibble()

## Count frequency of profiles
data2_high_counted <- plyr::count(data2_high[, ])

###### 5. Assessment of symptom phenotypes ###################################
######  5.1 Low  #############################################################
######   5.1.1 Phenotypes  ###################################################
## Number of unique phenotypes
nrow(data2_low_counted) # 150

## Number of endorsements of most common phenotype
max(data2_low_counted$freq) # 939
summary(data2_low_counted$freq)
######   5.1.2 Jaccard-Index  ################################################
data_jacc_dist <- dist(data2_low, method = "binary")
data_jacc_index <- 1-data_jacc_dist

summary(data_jacc_index) # Median = 0, Q1-Q3 = [0, 0.33]
hist(data_jacc_index)

######   5.1.3 Plot the phenotypes distribution  #############################
freq1_top  <- data2_low_counted %>% 
  top_n(freq, n = 100) %>% 
  select(freq) %>% 
  arrange(-freq)

pdf("Images/LOW_Phenotypes_PHQ9_Binarized.pdf", width=8, height=8)
ggplot(freq1_top, aes(x=as.factor(1:nrow(freq1_top)),y=freq)) +
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(" ") + 
  ylab("Freq") +
  ggtitle("LOW")+
  theme_minimal_hgrid() +
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())
dev.off() 

######  5.2 Medium  ##########################################################
######   5.2.1 Phenotypes  ###################################################
## Number of unique phenotypes
nrow(data2_med_counted) # 339

## Number of endorsements of most common phenotype
max(data2_med_counted$freq) # 352
summary(data2_med_counted$freq)
######   5.2.2 Jaccard-Index  ################################################
data_jacc_dist <- dist(data2_med, method = "binary")
data_jacc_index <- 1-data_jacc_dist

summary(data_jacc_index) # Median = 0.42, Q1-Q3 [0.33, 0.6]
hist(data_jacc_index)

######   5.2.3 Plot the phenotypes distribution  #############################
freq1_top  <- data2_med_counted %>% 
  top_n(freq, n = 100) %>% 
  select(freq) %>% 
  arrange(-freq)

pdf("Images/med_Phenotypes_PHQ9_Binarized.pdf", width=8, height=8)
ggplot(freq1_top, aes(x=as.factor(1:nrow(freq1_top)),y=freq)) +
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(" ") + 
  ylab("Freq") +
  ggtitle("MEDIUM")+
  theme_minimal_hgrid() +
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())
dev.off() 
######  5.3 High  ############################################################
######   5.3.1 Phenotypes  ###################################################
## Number of unique phenotypes
nrow(data2_high_counted) # 167

## Number of endorsements of most common phenotype
max(data2_high_counted$freq) # 1318
summary(data2_high_counted$freq)
######   5.3.2 Jaccard-Index  ################################################
data_jacc_dist <- dist(data2_high, method = "binary")
data_jacc_index <- 1-data_jacc_dist

summary(data_jacc_index) # Median = 0.75, Q1-Q3 [0.62, .85]
hist(data_jacc_index)

######   5.3.3 Plot the phenotypes distribution  #############################
freq1_top  <- data2_high_counted %>% 
  top_n(freq, n = 100) %>% 
  select(freq) %>% 
  arrange(-freq)

pdf("Images/high_Phenotypes_PHQ9_Binarized.pdf", width=8, height=8)
ggplot(freq1_top, aes(x=as.factor(1:nrow(freq1_top)),y=freq)) +
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(" ") + 
  ylab("Freq") +
  ggtitle("HIGH")+
  theme_minimal_hgrid() +
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())
dev.off() 

###### 6. Test distributions #################################################
######  6.1 Low ##############################################################
#### Prepare
Distribution <- data2_low_counted$freq
### Power Law
m_pl = displ$new(Distribution)
est_pl = estimate_xmin(m_pl)
m_pl$setXmin(est_pl)

# Estimated Parameters
m_pl$xmin # 105
m_pl$pars # 2.62

## Bootstrap parameters
## Test whether power law is possible
bs_p = bootstrap_p(m_pl, no_of_sims = 5000, threads = 5, seed = 241)
bs_p$p # 0.2776

# SD 
sd(bs_p$bootstraps$xmin)
sd(bs_p$bootstraps$pars)

pdf("Images/LOW_PL_parameters_boot_PHQ9_Binarized.pdf", width=8, height=8)
plot(bs_p)
dev.off() 

### Test if power law or log-normal distribution fits the UPPER TAIL better
## Log normal with Xmin of PL
m_ln_EQ = dislnorm$new(Distribution) 
m_ln_EQ$setXmin(m_pl$getXmin())
est_m_ln_EQ = estimate_pars(m_ln_EQ)
m_ln_EQ$setPars(est_m_ln_EQ)

# Plot different distributions
options(scipen=5)
pdf("Images/LOW_PL_PHQ9Binarized.pdf", width=8, height=8)
plot(m_pl, xlab = "", ylab="CDF",panel.first = grid(col = "grey80"))
lines(m_pl, col = 2,lty = 1, lwd = 2) 
lines(m_ln_EQ, col = 4,lty = 2, lwd = 2) 
dev.off()

# Formally assess
compare_distributions(m_pl, m_ln_EQ)$p_two_sided # p < 0.05 -> one of the two has better fit
compare_distributions(m_pl, m_ln_EQ)$p_one_sided #   p < 0.05 -> m_pl  better fit
compare_distributions(m_ln_EQ, m_pl)$p_one_sided #   p < 0.05 -> m_ln_EQ better fit

######  6.2 Medium ##############################################################
#### Prepare
Distribution <- data2_med_counted$freq#[data2_med_counted$freq > 7]
### Power Law
m_pl = displ$new(Distribution)
est_pl = estimate_xmin(m_pl)
m_pl$setXmin(est_pl)

# Estimated Parameters
m_pl$xmin # 20
m_pl$pars # 2.04

## Bootstrap parameters
## Test whether power law is possible
bs_p = bootstrap_p(m_pl, no_of_sims = 5000, threads = 5, seed = 241)
bs_p$p # 0.026 - so, not conforming with powerlaw

# SD 
sd(bs_p$bootstraps$xmin)
sd(bs_p$bootstraps$pars)

pdf("Images/MED_PL_parameters_boot_PHQ9Binarized.pdf", width=8, height=8)
plot(bs_p)
dev.off() 

### Test if power law or log-normal distribution fits the UPPER TAIL better
## Log normal with Xmin of PL
m_ln_EQ = dislnorm$new(Distribution) 
m_ln_EQ$setXmin(m_pl$getXmin())
est_m_ln_EQ = estimate_pars(m_ln_EQ)
m_ln_EQ$setPars(est_m_ln_EQ)

# Plot different distributions
options(scipen=5)
pdf("Images/MED_PL_PHQ9Binarized.pdf", width=8, height=8)
plot(m_pl, xlab = "", ylab="CDF",panel.first = grid(col = "grey80"))
lines(m_pl, col = 2,lty = 1, lwd = 2) 
lines(m_ln_EQ, col = 4,lty = 2, lwd = 2) 
dev.off()

# Formally assess
compare_distributions(m_pl, m_ln_EQ)$p_two_sided # p < 0.05 -> one of the two has better fit
compare_distributions(m_pl, m_ln_EQ)$p_one_sided #   p < 0.05 -> m_pl  better fit
compare_distributions(m_ln_EQ, m_pl)$p_one_sided #   p < 0.05 -> m_ln_EQ better fit

######  6.3 High ##############################################################
#### Prepare
Distribution <- data2_high_counted$freq
### Power Law
m_pl = displ$new(Distribution)
est_pl = estimate_xmin(m_pl)
m_pl$setXmin(est_pl)

# Estimated Parameters
m_pl$xmin # 14
m_pl$pars # 1.77

## Bootstrap parameters
## Test whether power law is possible
bs_p = bootstrap_p(m_pl, no_of_sims = 5000, threads = 5, seed = 241)
bs_p$p # 0.242

# SD 
sd(bs_p$bootstraps$xmin)
sd(bs_p$bootstraps$pars)

pdf("Images/HIGH_PL_parameters_boot_PHQ9Binarized.pdf", width=8, height=8)
plot(bs_p)
dev.off() 

### Test if power law or log-normal distribution fits the UPPER TAIL better
## Log normal with Xmin of PL
m_ln_EQ = dislnorm$new(Distribution) 
m_ln_EQ$setXmin(m_pl$getXmin())
est_m_ln_EQ = estimate_pars(m_ln_EQ)
m_ln_EQ$setPars(est_m_ln_EQ)

# Plot different distributions
options(scipen=5)
pdf("Images/HIGH_PL_PHQ9Binarized.pdf", width=8, height=8)
plot(m_pl, xlab = "", ylab="CDF",panel.first = grid(col = "grey80"))
lines(m_pl, col = 2,lty = 1, lwd = 2) 
lines(m_ln_EQ, col = 4,lty = 2, lwd = 2) 
dev.off()

# Formally assess
compare_distributions(m_pl, m_ln_EQ)$p_two_sided # p < 0.05 -> one of the two has better fit
compare_distributions(m_pl, m_ln_EQ)$p_one_sided #   p < 0.05 -> m_pl  better fit
compare_distributions(m_ln_EQ, m_pl)$p_one_sided #   p < 0.05 -> m_ln_EQ better fit


######  7. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
