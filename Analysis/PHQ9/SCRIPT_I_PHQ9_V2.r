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
#                               DATA: PHQ9                                   #
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


library(Hmisc)
library(corrplot)


###### 2. Import and prepare data ############################################
#### Import
# PATH
#df <- read.spss('/home/or/Documents/pcl5_vaData/phq_pcl_19/PHQpct19.sav', to.data.frame = TRUE)
df <- read.csv('PHQ_MDD.csv')

# check depression dx
table(df$DEPRESSION_DX) # all diagnoserd with MDD
# grab just PHQ items
data_subset <- df[,9:17]
# set all items as numeric
data_subset <- data_subset %>%
  mutate_if(is.character, as.numeric)

dfPHQ <- data_subset
dfPHQ$sex <- df$GENDER # 
dfPHQ$age <- df$AGE
# omitting data just based on PCL NAshow to wake up files in cli ubuntu?

# set all PHQ items to numeric

#### Prepare dataset
dfPHQ <- dfPHQ[complete.cases(data_subset), ] # 170,035

### Clean dataset


###### 2.1 Select variables for methods and materials // DATA0
data0 <- dfPHQ
 
data0$sex <- as.factor(data0$sex)


###### 2.2 Select & prepare dataset for further analysis // DATA1
data1 <- data0[, 1:9]
## Extract variable names
variable_names <- c(names(data1), "total")

## Rename variables to "Q1:QN" for binarization
colnames(data1) <- c(paste0("Q", 1:(ncol(data1))))

## Add total score
data1 <- data1 %>% 
  mutate(total = rowSums(data1[1:ncol(data1)]))

#### Correlation Matrix #######
dataCor <- data0[1:nrow(data0),1:9]
correlations <- rcorr(as.matrix(dataCor))

# Create a mask for significant correlations
cor_mat <- correlations$r  # Correlation matrix
p_mat <- correlations$P  # P-value matrix
sig_level <- 0.05  # Significance level
mask <- p_mat <= sig_level  # Matrix to mask non-significant correlations

# Plot using corrplot
corrplot::corrplot(cor_mat, type = "upper", order = "hclust", 
                   p.mat = p_mat, sig.level = sig_level, 
                   insig = "blank", addCoef.col = "black", # Show correlation coefficient
                   tl.col="black", tl.srt=45) # Text label color and rotation


#########
###### 3. Descriptive #######################################################
###### 3.1 For Material & Methods 
# Gender
summary(data0$sex) # 130534 = male, 39501 = female

# Age
summary(data0$age)# median = 53.00, Mean = 52.12, Q1-Q3 [39.00,64.00]. Range 19 - 110

# Range overall score
summary(data1$total)

# Cronbach's alpha
# Overall
psych::alpha(subset(data1, select = (-total))) # 0.87

###### 3.2 For further analysis
summary(data1)
hist(data1$total)


###### 4. Binarizing & create datax ###########################################
######  4.1 Binarizing ########################################################
## set cut-off
## We do not binarize PHQ data
cut_off <- 1 #will be used with <= // Specify for individual analysis

## Binarize
# data1_binarized <- data1
# for (i in 1:(ncol(data1)-1)){
#   orig <- paste("q", i, sep = "")
#   bin <- paste("Q", i, sep = "")
#   data1_binarized[orig] <- case_when(data1_binarized[bin]<= cut_off ~ 0, data1_binarized[bin]>cut_off ~ 1)  #0 = "Symptom absent", 1 = "Symptom present"
# 
# }

data1_binarized <- data1 %>%
  mutate(across(starts_with("Q"), ~if_else(. <= cut_off, 0, 1), .names = "q{str_remove(.col, 'Q')}"))
  

# Create new data frame
data2 <- data1_binarized %>% 
  select(total:ncol(data1_binarized)) %>% 
  select(-total) %>% 
  tibble()


## Count frequency of profiles
data2_counted <- plyr::count(data2[, ]) # 505

# Create sum score of endorsed symptoms
data2_counted <- data2_counted %>% 
  mutate(total_bin = rowSums(data2_counted)-freq)


######  4.2 Create datax ########################################################
# Create full dataset
datax <- dplyr::left_join(data1_binarized, data2_counted)

# Save for further analysis
write_csv2(data1, "Analysis/PHQ9/Generated Data/binarized.csv")
write_csv2(data2_counted, "Analysis/PHQ9/Generated Data/Binarized_freq_count.csv")
write_csv2(datax, "Analysis/PHQ9/Generated Data/Binarized_Matched_freq_count.csv")


######  5. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
