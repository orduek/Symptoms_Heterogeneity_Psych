##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 2.1 (15.10.2021)                        #
#                                                                            #
#----------------------------------------------------------------------------#
#                                                                            #
#                       Script I -  Import and Binarize                      #
#									                                                           #
#----------------------------------------------------------------------------#
#                                                                            #
#                              DATA: PANSS                                   #
#									                                                           #
##############################################################################


###### Table of Contents #####################################################
#----- 1. Load libraries ----------------------------------------------------#
#----- 2. Import and prepare data -------------------------------------------#
#-----  2.1 Select variables for methods and materials ----------------------#
#-----  2.2 Select & prepare dataset for further analysis -------------------#
#----- 3. Descriptive -------------------------------------------------------#
#----- 4. Binarizing & create datax  ----------------------------------------#
#-----  4.1 Binarizing  ------------ ----------------------------------------#
#-----  4.2 Create datax  ---------------------------------------------------#
#----- 5. Session Info  -----------------------------------------------------#


## General Notes ##
# protector.all = raw data 
# data0 = 
# data1 = Extracted variables prepared for further analysis
# data2 = binarized dataframe
# datax = binarized & original items & frequency
# variable_names = names of variable of data 1

###### 1. Load Libraries #####################################################
# Data handling + basic calculations
library("tidyverse")
library("readxl")
library("foreign")
library("ReIns")
library("psych")

###### 2. Import and prepare data ############################################
#### Import
# PATH
df <- read.csv('pnass01.csv')
#### Prepare dataset
# Descriptives
df$Age <- as.numeric(df$interview_age) / 10 # change age variable accordingly
panssDf <- df[,213:226]
panssDf$subjectkey <- df$subjectkey
panssDf$age <- df$Age
panssDf$sex <- df$sex
### Clean dataset
panss <- panssDf[!duplicated(panssDf$subjectkey),]
# clean NAs
data_subset <- panss[,1:14]

panss <- panss[complete.cases(data_subset),] # only 4616 valid cases

###### 2.1 Select variables for methods and materials // DATA0
data0 <- panss
 
data0$sex <- as.factor(data0$sex)


###### 2.2 Select & prepare dataset for further analysis // DATA1
data1 <- data0[,1:14]

## Extract variable names
variable_names <- c(names(data1), "total")

## Rename variables to "Q1:QN" for binarization
colnames(data1) <- c(paste0("Q", 1:(ncol(data1))))

## Add total score
data1 <- data1 %>% 
  mutate(total = rowSums(data1[1:ncol(data1)]))


###### 3. Descriptive #######################################################
###### 3.1 For Material & Methods 
# Gender
summary(data0$sex) # 2963 = male, 1653 = female

# Age
summary(data0$age) # median=38.6, Mean= 41.8, q1-q3 28.8,55.2 range 25-79

# Range overall score
summary(data1$total) # Median = 33, Mean = 33.24, q1-q3 = 26.0,40.0

# Cronbach's alpha
# Overall
psych::alpha(subset(data1, select = (-total)))

###### 3.2 For further analysis
summary(data1)
hist(data1$total)


###### 4. Binarizing & create datax ###########################################
######  4.1 Binarizing ########################################################
## set cut-off
cut_off <- 3 #will be used with <= // Specify for individual analysis

## Binarize
data1_binarized <- data1
for (i in 1:(ncol(data1)-1)){
  orig <- paste("q", i, sep = "")
  bin <- paste("Q", i, sep = "")
  data1_binarized[orig] <- dplyr::case_when(data1_binarized[bin]<= cut_off ~ 0, data1_binarized[bin]>cut_off ~ 1)  #0 = "Symptom absent", 1 = "Symptom present"
  
}

# Create new data frame
data2 <- data1_binarized %>% 
  select(total:ncol(data1_binarized)) %>% 
  select(-total) %>% 
  tibble()

## Count frequency of profiles
data2_counted <- plyr::count(data2[, ])

# Create sum score of endorsed symptoms
data2_counted <- data2_counted %>% 
  mutate(total_bin = rowSums(data2_counted)-freq)


######  4.2 Create datax ########################################################
# Create full dataset
datax <- dplyr::left_join(data1_binarized, data2_counted)

# Save for further analysis
write_csv2(data1_binarized, "Analysis/PANSS/Generated Data/binarized.csv")
write_csv2(data2_counted, "Analysis/PANSS/Generated Data/freq_count.csv")
write_csv2(datax, "Analysis/PANSS/Generated Data/Matched_freq_count.csv")


######  5. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
