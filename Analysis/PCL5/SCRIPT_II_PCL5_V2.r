##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 3.4 (31.12.2022)                        #
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

median(datax$total)
quantile(datax$total)  

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

## Assess the ten most common phenotypes
data2_counted <- data2_counted %>% 
  arrange(desc(freq))


print(data2_counted[1,]) 
print(data2_counted[2,]) 
print(data2_counted[3,]) 
print(data2_counted[4,]) 
print(data2_counted[5,]) 
print(data2_counted[6,]) 
print(data2_counted[7,]) 
print(data2_counted[8,]) 
print(data2_counted[9,]) 
print(data2_counted[10,]) 

gt(data2_counted[1:10,1:20])%>% gtsave('Analysis/PCL5/Generated Data/ten_common_pcl5.rtf')#'item2','item3','item4','item5','item6','item7','item8','item9','item10')

## Median endorsement of phenotypes   ?!?!?!?!?!?!?!?!?
summary(datax$freq) # median = 2453, Q1=25, Q3 = 20575
hist(datax$freq) # plot

## Number of  phenotypes,endorsed less than 6 times
data_uncommon <- data2_counted %>% 
  filter (freq < 6)


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
pdf("Images/Top_100_Phenotypes_PCL_5.pdf", width=8, height=8)
ggplot(freq1_top, aes(x=as.factor(1:nrow(freq1_top)),y=freq)) +
  geom_hline(yintercept = c((median(datax$freq)), (max(freq1_top$freq))), color = "gray", size = 0.3) + #max and median
  geom_bar(stat = "identity",fill = c('red',"slateblue")) +
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
bs_p$p # 0.861

# Estimated Parameters
m_pl$xmin # Xmin 8
m_pl$pars # alpha 1.86

bs = bootstrap(m_pl, no_of_sims = 5000, threads = 10, seed = 241)

# SD 
sd(bs$bootstraps$xmin) # 2.29
sd(bs$bootstraps$pars) # 0.079

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
bs_ln_p = bootstrap_p(m_ln_EQ, no_of_sims = 5000, threads = 10, seed = 241)
bs_ln_p$p # 0.507

bs_ln = bootstrap(m_ln_EQ, no_of_sims = 5000, threads = 10, seed = 241)

# Parameters
m_ln_EQ$xmin
m_ln_EQ$pars[[1]] # -27.53
m_ln_EQ$pars[[2]] # 6.079

# SD 
sd(bs_ln$bootstraps$xmin) #
sd(bs_ln$bootstraps$pars1) # 72.50
sd(bs_ln$bootstraps$pars2) # 1.23

## Exponential with Xmin of PL
m_ex_EQ = disexp$new(Distribution) 
m_ex_EQ$setXmin(m_pl$getXmin())
est_m_ex_EQ = estimate_pars(m_ex_EQ)
m_ex_EQ$setPars(est_m_ex_EQ)

## Bootstrap parameters

bs_ex_p = bootstrap_p(m_ex_EQ, no_of_sims = 5000, threads = 10, seed = 241)
bs_ex_p$p # 0

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
plot(compare_distributions(m_pl, m_ex_EQ))
compare_distributions(m_ex_EQ, m_ln_EQ)$p_two_sided # p < 0.12 -> one of the two has better fit
compare_distributions(m_pl, m_ex_EQ)$p_one_sided

compare_distributions(m_pl, m_ex_EQ)$p_one_sided #   p < 0.063 -> m_pl  better fit
compare_distributions(m_ln_EQ, m_ex_EQ)$p_one_sided #   p < 0.063 -> m_ln_EQ better fit


### Plot 100 most common and stack plot ####

source('plotting_functions.r')
png('Images/hist_PCL5_top100.png')
plotHundred(nCommon = 10, freq1_top = freq1_top)# %>% ggsave('Images/hist_PCL5_top100.png')
dev.off()
png('Images/stackedBar_PCL5.png')
stackedPlot(freq1_top = freq1_top, 10, data2_counted = data2_counted, datax = datax)
dev.off()
################################################
## Calculating prevalence of specific symptom ##
################################################
sum(data2_counted$q1) / nrow(data2_counted) # 0.747

sum(datax$q1) / nrow(datax) # 0.9449
# go over each item
h <- matrix(nrow = 20, ncol = 2)
for (i in 1:20) {
  h[i,2] <- sum(data2_counted[,i]) / nrow(data2_counted)  
  h[i,1] <- i
  
}
gt(data.frame(h)) %>% gtsave('Analysis/PCL5/Generated Data/frequency_perProfile_PCL.rtf')
# do it per person

l <- matrix(nrow=20, ncol=2)
for (i in 22:41) {
  l[i-21,2] <- sum(datax[,i]) / nrow(datax)
  l[i-21,1] <- i-21
}

gt(data.frame(l)) %>% gtsave('Analysis/PCL5/Generated Data/frequency_perPerson_PCL.rtf')
#######


######  6. Export data for Figures ##############################################
### Figure 1a
freq1_top_PCL <- freq1_top 
save(freq1_top_PCL , file = "Analysis/PCL5/Generated Data/freq1_top_PCL.RData")

### Figure 1b
# Export m_pl & m_ln
res_pl_PCL <- plot(m_pl)
line_pl_PCL <- lines(m_pl)
line_ln_PCL <- lines(m_ln_EQ)
line_ex_PCL<- lines(m_ex_EQ)

save(res_pl_PCL, file = "Analysis/PCL5/Generated Data/res_pl_PCL.RData")
save(line_pl_PCL, file = "Analysis/PCL5/Generated Data/line_pl_PCL.RData")
save(line_ln_PCL, file = "Analysis/PCL5/Generated Data/line_ln_PCL.RData")
save(line_ex_PCL, file = "Analysis/PCL5/Generated Data/line_ex_PCL.RData")

######  7. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
