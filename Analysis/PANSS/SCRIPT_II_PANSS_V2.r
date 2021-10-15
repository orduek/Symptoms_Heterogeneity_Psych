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
#                     Script II - Descriptive & Distribution                 #
#									                                                           #
#----------------------------------------------------------------------------#
#                                                                            #
#                                 PANSS                                      #
#									                                                           #
##############################################################################


###### Table of Contents #####################################################
#----- 1. Load libraries ----------------------------------------------------#
#----- 2. Import and prepare data -------------------------------------------#
#----- 3. Descriptive -------------------------------------------------------#
#----- 4. Count symptom profiles --------------------------------------------#
#-----  4.1 Specific phenotypes ---------------------------------------------#
#-----  4.2 Jaccard-Index ---------------------------------------------------#
#-----  4.3 Plot the phenotypes distribution --------------------------------#
#----- 5. Test distributions ------------------------------------------------#
#----- 6.  Export data for Figures  -----------------------------------------#
#----- 7. Session Info  -----------------------------------------------------#

## General Notes ##
# data2 = binarized dataframe
# datax = dataframe script one with scores, binarized items and frequency profiles

###### 1. Load Libraries #####################################################
# Data handling + basic calculations
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("foreign")) install.packages("foreign")
if(!require("ReIns")) install.packages("ReIns")

#Power Law
if(!require("poweRlaw")) install.packages("poweRlaw")


###### 2. Import and prepare data ############################################
## Prepare
# Load datax & data2 created in script one
data1_binarized<- read_delim("Analysis/PANSS/Generated Data/binarized.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)

data2_counted<- read_delim("Analysis/PANSS/Generated Data/freq_count.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

datax<- read_delim("Analysis/PANSS/Generated Data/Matched_freq_count.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

###### 3. Descriptive #######################################################
## Datax
summary(datax)
nrow(datax)  # 4616

# Summed severity
hist(datax$total)
mean(datax$total) # 33.23
sd(datax$total) # 10.23

# Summed severity of binarized items
hist(datax$total_bin)
summary(datax$total_bin)

###### 4. Assessment of symptom phenotypes ###################################
######  4.1 Specific phenotypes  #############################################
## Number of unique phenotypes
nrow(data2_counted) # 1185

## Number of endorsements of most common phenotype
max(data2_counted$freq) # 950

## Assess the three most common phenotypes
data2_counted <- data2_counted %>% 
  arrange(desc(freq))

print(data2_counted[1,]) # all zero
print(data2_counted[2,]) # zero expect q12 (negative symptom)
print(data2_counted[3,]) # zero expect q3 (positive)

## Median endorsement of phenotypes   ?!?!?!?!?!?!?!?!?
summary(datax$freq) # median = 16, q1-q3 = 3-239
hist(datax$freq) # plot

######  4.2 Jaccard-Index  ###################################################
data_jacc_dist <- dist(data1_binarized, method = "binary")
data_jacc_index <- 1-data_jacc_dist

summary(data_jacc_index) # median = 0.789, q1-q3 [0.72, 0.869]
hist(data_jacc_index)

### Jaccard truncated#####
truncDat <- filter(datax, freq <=16) # using median frequency
truncDat <- truncDat[,16:29] # take only the binarized questionnaires
data_jacc_dist <- dist(truncDat, method = "binary")
data_jacc_index <- 1-data_jacc_dist

summary(data_jacc_index) # median = 0.250, q1-q3 [0.143, 0.375]
hist(data_jacc_index)

######  4.3 Plot the phenotypes distribution #################################
### 4.3.1 Plot the 100 most common phenotypes
freq1_top  <- data2_counted %>% 
  top_n(freq, n = 100) %>% 
  select(freq)

# The frequency of the fifty most common symptom combinations
pdf("Top_100_Phenotypes_PANSS.pdf", width=8, height=8)
ggplot(freq1_top, aes(x=as.factor(1:nrow(freq1_top)),y=freq)) +
  geom_hline(yintercept = c((median(datax$freq)), (max(freq1_top$freq))), color = "grey", size = 0.3) + #max and median
  geom_bar(stat = "identity",fill = "grey26") +
  xlab(" ") + 
  ylab("Number of endorsements") +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks=c(round((median(datax$freq)), 0), round((max(freq1_top$freq)), 0)))  #max and median
dev.off() 

###### 5. Test distributions #################################################
#### Prepare
Distribution <- data2_counted$freq
### Power Law
m_pl = displ$new(Distribution)
est_pl = estimate_xmin(m_pl)
m_pl$setXmin(est_pl)

# Estimated Parameters
m_pl$xmin # 1
m_pl$pars # 2.07

## Bootstrap parameters
## Test whether power law is possible
bs_p = bootstrap_p(m_pl, no_of_sims = 5000, threads = 10, seed = 241)
bs_p$p # 0.222

# SD 
sd(bs_p$bootstraps$xmin)
sd(bs_p$bootstraps$pars)

pdf("PL_parameters_boot_PANSS.pdf", width=8, height=8)
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
pdf("PL_ML_CDF_equal_Xmin_PANSS.pdf", width=8, height=8)
plot(m_pl, xlab = "", ylab="CDF",panel.first = grid(col = "grey80"))
lines(m_pl, col = 2,lty = 1, lwd = 2) 
lines(m_ln_EQ, col = 4,lty = 2, lwd = 2) 
dev.off()

# Formally assess
compare_distributions(m_pl, m_ln_EQ)$p_two_sided # p < 0.05 -> one of the two has better fit
compare_distributions(m_pl, m_ln_EQ)$p_one_sided #   p < 0.05 -> m_pl  better fit
compare_distributions(m_ln_EQ, m_pl)$p_one_sided #   p < 0.05 -> m_ln_EQ better fit


######  6. Export data for Figures ##############################################
### Figure 1a
freq1_top_PANSS <- freq1_top 
save(freq1_top_PANSS , file = "Analysis/PANSS/Generated Data/freq1_top_PANSS.RData")

### Figure 1b
# Export m_pl & m_ln
res_pl_PANSS <- plot(m_pl)
line_pl_PANSS <- lines(m_pl)
line_ln_PANSS <- lines(m_ln_EQ)

save(res_pl_PANSS, file = "Analysis/PANSS/Generated Data/res_pl_PANSS.RData")
save(line_pl_PANSS, file = "Analysis/PANSS/Generated Data/line_pl_PANSS.RData")
save(line_ln_PANSS, file = "Analysis/PANSS/Generated Data/line_ln_PANSS.RData")

######  7. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
