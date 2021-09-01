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
#                               DATA: DASS                                   #
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
#----- 6. Export data for Figures -------------------------------------------#
#----- 7. Session Info  -----------------------------------------------------#

## General Notes ##
# data2 = binarized dataframe
# datax = dataframe script one with scores, binarized items and frequency profiles
# datay = dataframe of items with ordinal ratings AND frequency of symptom profiles (reduced datax)
# dataz = dataframe of items with ordinal ratings WITHOUT frequency of symptom profiles (reduced datay)

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
data1_binarized<- read_delim("Generated_Data/DASS_binarized.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)

data2_counted<- read_delim("Generated_Data/DASS_freq_count.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

datax<- read_delim("Generated_Data/DASS_Matched_freq_count.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

###### 3. Descriptive #######################################################
## Datax
summary(datax)
nrow(datax)  #sample size

# Summed severity
hist(datax$total)
mean(datax$total)
sd(datax$total)

# Summed severity of binarized items
hist(datax$total_bin)
summary(datax$total_bin)

###### 4. Assessment of symptom phenotypes ###################################
######  4.1 Specific phenotypes  #############################################
## Number of unique phenotypes
nrow(data2_counted) #

## Number of endorsements of most common phenotype
max(data2_counted$freq) #

## Assess the three most common phenotypes
data2_counted <- data2_counted %>% 
  arrange(desc(freq))

print(data2_counted[1,]) # 
print(data2_counted[2,]) # 
print(data2_counted[3,]) # 

## Median endorsement of phenotypes   ?!?!?!?!?!?!?!?!?
summary(datax$freq) # median 49
hist(datax$freq) # plot

######  4.2 Jaccard-Index  ###################################################
#data1_binarized_selected <- data1_binarized %>%   #Needed to select, because dataframe is too large for my memory
# sample_n(1000)

data_jacc_dist <- dist(data1_binarized, method = "binary")
data_jacc_index <- 1-data_jacc_dist

summary(data_jacc_index) # 
median(data_jacc_index)
hist(data_jacc_index)

### Jaccard truncated#####
truncDat <- filter(datax, freq <=49) # using median frequency
truncDat <- truncDat[,15:27]
data_jacc_dist <- dist(truncDat, method = "binary")
data_jacc_index <- 1-data_jacc_dist

summary(data_jacc_index) # median = 0.33, q1-q3 [0.200, 0.454]
hist(data_jacc_index)

######  4.3 Plot the phenotypes distribution #################################
### 4.3.1 Plot the 100 most common phenotypes
freq1_top  <- data2_counted %>% 
  top_n(freq, n = 100) %>% 
  select(freq)

# The frequency of the fifty most common symptom combinations
pdf("Images/Top_100_Phenotypes_DASS.pdf", width=8, height=8)
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
m_pl$xmin # Xmin
m_pl$pars # alpha

## Bootstrap parameters
## Test whether power law is possible
# bs_p = bootstrap_p(m_pl, no_of_sims = 5000, threads = 5, seed = 241)
bs_p$p

pdf("Images/PL_parameters_boot_DASS.pdf", width=8, height=8)
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
pdf("Images/PL_ML_CDF_equal_Xmin_DASS.pdf", width=8, height=8)
plot(m_pl, xlab = "", ylab="CDF",panel.first = grid(col = "grey80"))
lines(m_pl, col = 2,lty = 1, lwd = 6) 
lines(m_ln_EQ, col = 4,lty = 2, lwd = 6) 
dev.off()

# Formally assess
compare_distributions(m_pl, m_ln_EQ)$p_two_sided # p < 0.05 -> one of the two has better fit
compare_distributions(m_pl, m_ln_EQ)$p_one_sided #   p < 0.05 -> m_ln_EQ  better fit
compare_distributions(m_ln_EQ, m_pl)$p_one_sided #   p < 0.05 -> m_pl better fit

######  6. Export data for Figures ##############################################
### Figure 1a
freq1_top_DASS <- freq1_top 
save(freq1_top_DASS, file = "Generated Data/freq1_top_DASS.RData")

### Figure 1b
# Export m_pl & m_ln
res_pl_DASS <- plot(m_pl)
line_pl_DASS <- lines(m_pl)
line_ln_DASS <- lines(m_ln_EQ)

save(res_pl_DASS, file = "Generated Data/res_pl_DASS.RData")
save(line_pl_DASS, file = "Generated Data/line_pl_DASS.RData")
save(line_ln_DASS, file = "Generated Data/line_ln_DASS.RData")

######  7. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
