##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 3.1 (17.02.2021)                        #
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
library("tidyverse")
library("foreign")
library("ReIns")

#Power Law
library("poweRlaw")


###### 2. Import and prepare data ############################################
## Prepare
# Load datax & data2 created in script one
data1_binarized<- read_delim("Analysis/DASS/Generated_Data/DASS_binarized.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)

data2_counted<- read_delim("Analysis/DASS/Generated_Data/DASS_freq_count.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

datax<- read_delim("Analysis/DASS/Generated_Data/DASS_Matched_freq_count.csv", 
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

# top ten 
gt(data2_counted[1:10,1:13]) %>% gtsave('Analysis/DASS/topTenBinarizedDASS.rtf')

## Median endorsement of phenotypes  
summary(datax$freq) # median 49
hist(datax$freq) # plot

######  4.3 Plot the phenotypes distribution #################################
### 4.3.1 Plot the 100 most common phenotypes
freq1_top  <- data2_counted %>% 
  top_n(freq, n = 100) %>% 
  select(freq)



# symptoms reported less or equal to 5 time
data2_counted %>% filter(freq<=5) %>% nrow() # 

# symptom profile reported once
data2_counted %>% filter(freq==1) %>% nrow() # 

# individuals reporting one of the ten most common symptoms
data2_counted[1:10,] %>% summarise(sum(freq))
# all the rest
data2_counted[11:nrow(data2_counted),] %>% summarise(sum(freq))

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

## Bootstrap parameters
## Test whether power law is possible
bs_p = bootstrap_p(m_pl, no_of_sims = 5000, threads = 10, seed = 241)
bs_p$p # 0.139

# Estimated Parameters
m_pl$xmin # Xmin 7
m_pl$pars # alpha 2.103

bs = bootstrap(m_pl, no_of_sims = 5000, threads = 10, seed = 241)
# SD 
sd(bs$bootstraps$xmin) # 3.47
sd(bs$bootstraps$pars) # 0.071

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
bs_ln = bootstrap(m_ln_EQ, no_of_sims = 5000, threads = 11, seed = 241)

# Parameters
m_ln_EQ$xmin #
m_ln_EQ$pars[[1]] # -16.98
m_ln_EQ$pars[[2]]# 4.32

# SD 
sd(bs_ln$bootstraps$xmin) # 0.84
sd(bs_ln$bootstraps$pars1)# 2.38
sd(bs_ln$bootstraps$pars2) # 0.36

## Exponential with Xmin of PL
m_ex_EQ = disexp$new(Distribution) 
m_ex_EQ$setXmin(m_pl$getXmin())
est_m_ex_EQ = estimate_pars(m_ex_EQ)
m_ex_EQ$setPars(est_m_ex_EQ)

## Bootstrap parameters
bs_ex = bootstrap(m_ex_EQ, no_of_sims = 5000, threads = 10, seed = 241)

# Parameters
m_ex_EQ$xmin
m_ex_EQ$pars

# SD 
sd(bs_ex$bootstraps$xmin) # 59.89
sd(bs_ex$bootstraps$pars) # 0.259

# Plot different distributions
options(scipen=5)
pdf("Images/PL_ML_CDF_equal_Xmin.pdf", width=8, height=8)
plot(m_pl, xlab = "", ylab="CDF",panel.first = grid(col = "grey80"))
lines(m_pl, col = "red",lty = 1, lwd = 2) 
lines(m_ln_EQ, col = "blue", lty = 2, lwd = 2) 
lines(m_ex_EQ, col = "orange",lty = 3, lwd = 2) 
dev.off()

# Formally assess
compare_distributions(m_pl, m_ln_EQ)$p_two_sided # p < 0.05 -> one of the two has better fit
compare_distributions(m_pl, m_ex_EQ)$p_two_sided # p < 0.05 -> one of the two has better fit
compare_distributions(m_ex_EQ, m_ln_EQ)$p_two_sided # p < 0.05 -> one of the two has better fit

compare_distributions(m_pl, m_ex_EQ)$p_one_sided #   p < 0.05 -> m_pl  better fit
compare_distributions(m_ln_EQ, m_ex_EQ)$p_one_sided #   p < 0.05 -> m_ln_EQ better fit


## Figures of combined data ####
source('plotting_functions.r')
png('Images/hist_DASS_top100.png')
plotHundred(nCommon = 10, freq1_top = freq1_top)# %>% ggsave('Images/hist_PCL5_top100.png')
dev.off()
png('Images/stackedBar_DASS.png')
stackedPlot(freq1_top = freq1_top, 10, data2_counted = data2_counted, datax)
dev.off()


################################################
## Calculating prevalence of specific symptom ##
################################################
sum(data2_counted$q1) / nrow(data2_counted) # 

sum(datax$q1) / nrow(datax) # 
# go over each item
h <- matrix(nrow = 13, ncol = 2)
for (i in 1:13) {
  h[i,2] <- sum(data2_counted[,i]) / nrow(data2_counted)  
  h[i,1] <- i
  
}
gt(data.frame(h)) %>% gtsave('Analysis/DASS/Generated_Data/frequency_perProfile_DASS.rtf')
# do it per person

l <- matrix(nrow=13, ncol=2)
for (i in 15:27) {
  l[i-14,2] <- sum(datax[,i]) / nrow(datax)
  l[i-14,1] <- i-14
}

gt(data.frame(l)) %>% gtsave('Analysis/DASS/Generated_Data/frequency_perPerson_DASS.rtf')
######  6. Export data for Figures ##############################################
### Figure 1a
freq1_top_DASS <- freq1_top 
save(freq1_top_DASS, file = "Generated Data/freq1_top_DASS.RData")

### Figure 1b
# Export m_pl & m_ln
res_pl_DASS <- plot(m_pl)
line_pl_DASS <- lines(m_pl)
line_ln_DASS <- lines(m_ln_EQ)
line_ex_DASS <- lines(m_ex_EQ)

save(res_pl_DASS, file = "Generated Data/res_pl_DASS.RData")
save(line_pl_DASS, file = "Generated Data/line_pl_DASS.RData")
save(line_ln_DASS, file = "Generated Data/line_ln_DASS.RData")
save(line_ex_DASS, file = "Generated Data/line_ex_DASS.RData")

######  7. Session info #########################################################
sessionInfo()

#####################################  END  ####################################
