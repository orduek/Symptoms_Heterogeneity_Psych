##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 2.2 (01.06.2021)                        #
#                                                                            #
#----------------------------------------------------------------------------#
#                                                                            #
#                       Script I -  Import and Binarize                      #
#									                                                           #
#----------------------------------------------------------------------------#
#                                                                            #
#                           DATA: PCL-5                                      #
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
dfPCL5 <- read.spss('/home/or/Documents/pcl5_vaData/phq_pcl_19/PCLpct19.sav', to.data.frame = TRUE)
#### Prepare dataset
data_subset <- dfPCL5[,5:24]
# omitting data just based on PCL NAs
dfPCL5 <- dfPCL5[complete.cases(data_subset), ]
dfPCL5$totalPCL <- dfPCL5$PCLN01 + dfPCL5$PCLN02 + dfPCL5$PCLN03 + dfPCL5$PCLN04 + dfPCL5$PCLN05 + 
  dfPCL5$PCLN06 + dfPCL5$PCLN07 + dfPCL5$PCLN08 + dfPCL5$PCLN09 + dfPCL5$PCLN10 + dfPCL5$PCLN11 +
  dfPCL5$PCLN12 + dfPCL5$PCLN13 + dfPCL5$PCLN14 + dfPCL5$PCLN15 + dfPCL5$PCLN16 + dfPCL5$PCLN17 +
  dfPCL5$PCLN18 + dfPCL5$PCLN19 + dfPCL5$PCLN20
## Datax

### Clean dataset

dfPCL5$FEMALE
mean(dfPCL5$AGE_OCT01)
summary(dfPCL5$AGE_OCT01)
sd(dfPCL5$AGE_OCT01)
mean(dfPCL5$totalPCL)
sd(dfPCL5$totalPCL)
hist(dfPCL5$totalPCL)
summary(dfPCL5$totalPCL)
table(dfPCL5$MALE)
###### 2.1 Select variables for methods and materials // DATA0
data0 <- dfPCL5 %>% # xxxxx = dataset after cleaning before selection
  select("AGE_OCT01", "FEMALE", "totalPCL") #include age, sex, variables of interest
 
data0$FEMALE <- as.factor(data0$FEMALE)


###### 2.2 Select & prepare dataset for further analysis // DATA1
data1 <- dfPCL5[,5:24] 

## Extract variable names
variable_names <- c(names(data1))

## Rename variables to "Q1:QN" for binarization
colnames(data1) <- c(paste0("Q", 1:(ncol(data1))))

## Add total score
total = rowSums(data1)
data1 <- cbind(data1, total)


###### 3. Descriptive #######################################################
###### 3.1 For Material & Methods 
# Gender
summary(data0$FEMALE) # 0 = male, 1 = female

# Age
summary(data0$AGE_OCT01)

# Range overall score
summary(data1$total)

# Cronbach's alpha
# Overall
psych::alpha(subset(data1, select = (-total)))

###### 3.2 For further analysis
summary(data1)
hist(data1$total)


###### 4. Binarizing & create datax ###########################################
######  4.1 Binarizing ########################################################
## set cut-off
cut_off <- 1 #will be used with <= // Specify for individual analysis

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
write_csv2(data1_binarized, "Analysis/PCL5/Generated Data/binarized.csv")
write_csv2(data2_counted, "Analysis/PCL5/Generated Data/freq_count.csv")
write_csv2(datax, "Analysis/PCL5/Generated Data/Matched_freq_count.csv")


######  5. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
