##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 3.0 (08.02.2022)                        #
#                                                                            #
#----------------------------------------------------------------------------#
#                                                                            #
#                     Script II - Descriptive & Distribution                 #
#									                                                           #
#----------------------------------------------------------------------------#
#                                                                            #
#                                  PCL5                                      #
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
library("tidyverse")
library("foreign")
library("ReIns")

#Power Law
library("poweRlaw")


###### 2. Import and prepare data ############################################
## Prepare
# Load datax & data2 created in script one
data1_binarized<- read_delim("Analysis/PCL5/Generated Data/binarized.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)

data2_counted<- read_delim("Analysis/PCL5/Generated Data/freq_count.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

datax<- read_delim("Analysis/PCL5/Generated Data/Matched_freq_count.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)


###### 3. Descriptive #######################################################
## Datax
summary(datax)
nrow(datax)  # 52,609

# Summed severity
hist(datax$total)
mean(datax$total) # 57.84
sd(datax$total) # 12.58

# Summed severity of binarized items
hist(datax$total_bin)
summary(datax$total_bin)

###### 4. Assessment of symptom phenotypes ###################################
######  4.1 Specific phenotypes  #############################################
## Number of unique phenotypes
nrow(data2_counted) # 8174

## Number of endorsements of most common phenotype
max(data2_counted$freq) # 20,575
summary(data2_counted$freq) # median 1, q1-q3 1,1
## Assess the three most common phenotypes
data2_counted <- data2_counted %>% 
  arrange(desc(freq))

print(data2_counted[1,]) # all symptoms
print(data2_counted[2,]) # all but PCL-16
print(data2_counted[3,]) # all but PCL- 8

## Median endorsement of phenotypes   ?!?!?!?!?!?!?!?!?
summary(datax$freq) # median = 2453, Q1=25, Q3 = 20575
hist(datax$freq) # plot

######  4.3 Plot the phenotypes distribution #################################
### 4.3.1 Plot the 100 most common phenotypes
freq1_top  <- data2_counted %>% 
  top_n(freq, n = 100) %>% 
  select(freq)

# The frequency of the fifty most common symptom combinations
pdf("Images/Top_100_Phenotypes_PCL_5.pdf", width=8, height=8)
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

## Bootstrap parameters
## Test whether power law is possible
bs_p = bootstrap_p(m_pl, no_of_sims = 5000, threads = 5, seed = 241)
bs_p$p 

# Estimated Parameters
m_pl$xmin # Xmin
m_pl$pars # alpha

# SD 
sd(bs_p$bootstraps$xmin) 
sd(bs_p$bootstraps$pars)

pdf("Images/PL_parameters_boot.pdf", width=8, height=8)
plot(bs_p)
dev.off() 

### Test if power law or log-normal distribution fits the UPPER TAIL better
## Log normal with Xmin of PL
m_ln_EQ = dislnorm$new(Distribution) 
m_ln_EQ$setXmin(m_pl$getXmin())
est_m_ln_EQ = estimate_pars(m_ln_EQ)
m_ln_EQ$setPars(est_m_ln_EQ)

## Bootstrap parameters
bs_ln = bootstrap(m_ln_EQ, no_of_sims = 5000, threads = 5, seed = 241)

# Parameters
m_ln_EQ$xmin
m_ln_EQ$pars[[1]]
m_ln_EQ$pars[[2]]

# SD 
sd(bs_ln$bootstraps$xmin) #
sd(bs_ln$bootstraps$pars1)
sd(bs_ln$bootstraps$pars2)

## Exponential with Xmin of PL
m_ex_EQ = disexp$new(Distribution) 
m_ex_EQ$setXmin(m_pl$getXmin())
est_m_ex_EQ = estimate_pars(m_ex_EQ)
m_ex_EQ$setPars(est_m_ex_EQ)

## Bootstrap parameters
bs_ex = bootstrap(m_ex_EQ, no_of_sims = 5000, threads = 5, seed = 241)

# Parameters
m_ex_EQ$xmin
m_ex_EQ$pars

# SD 
sd(bs_ex$bootstraps$xmin) #
sd(bs_ex$bootstraps$pars)

# Plot different distributions
options(scipen=5)
pdf("Images/PL_ML_CDF_equal_Xmin_PCL5.pdf", width=8, height=8)
plot(m_pl, xlab = "", ylab="CDF",panel.first = grid(col = "grey80"))
lines(m_pl, col = "red",lty = 1, lwd = 2) 
lines(m_ln_EQ, col = "blue", lty = 2, lwd = 2) 
lines(m_ex_EQ, col = "orange",lty = 3, lwd = 2) 
dev.off()

# Formally assess
compare_distributions(m_pl, m_ln_EQ)$p_two_sided # p < 0.95 -> one of the two has better fit
compare_distributions(m_pl, m_ex_EQ)$p_two_sided # p < 0.12 -> one of the two has better fit
compare_distributions(m_ex_EQ, m_ln_EQ)$p_two_sided # p < 0.12 -> one of the two has better fit

compare_distributions(m_pl, m_ex_EQ)$p_one_sided #   p < 0.063 -> m_pl  better fit
compare_distributions(m_ln_EQ, m_ex_EQ)$p_one_sided #   p < 0.063 -> m_ln_EQ better fit


######  6. Export data for Figures ##############################################
### Figure 1a
freq1_top_PCL <- freq1_top 
save(freq1_top_PCL , file = "Analysis/PCL5/Generated Data/freq1_top_PCL.RData")

### Figure 1b
# Export m_pl & m_ln
res_pl_PCL <- plot(m_pl)
line_pl_PCL <- lines(m_pl)
line_ln_PCL <- lines(m_ln_EQ)
line_ex_PCL <- lines(m_ex_EQ)

save(res_pl_PCL, file = "Analysis/PCL5/Generated Data/res_pl_PCL.RData")
save(line_pl_PCL, file = "Analysis/PCL5/Generated Data/line_pl_PCL.RData")
save(line_ln_PCL, file = "Analysis/PCL5/Generated Data/line_ln_PCL.RData")
save(line_ex_PCL, file = "Analysis/PCL5/Generated Data/line_ex_PCL.RData")

######  7. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
