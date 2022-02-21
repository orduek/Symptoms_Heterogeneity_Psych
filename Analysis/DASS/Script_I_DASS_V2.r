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
#                               DATA: DASS                                   #
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
#### Import
#data <- read_delim("Raw Data/DASS_data_21.02.19/data.csv", escape_double = FALSE, trim_ws = TRUE)
data <- read_delim("~/Desktop/data.csv", escape_double = FALSE, trim_ws = TRUE, delim = '\t')

###### 2.1 Select variables for methods and materials // DATA0
data0 <- data %>% 
  select(ends_with("A"), gender, age) %>% 
  filter(gender > 0) %>%  #67 with 0,  1=Male, 2=Female, 3=Other
  filter(age < 95) %>% 
  drop_na()

data0$gender <- as.factor(data0$gender)


###### 2.2 Select & prepare dataset for further analysis // DATA1
data_0_rename <- data0 %>% 
  select(Q1A:Q42A)

colnames(data_0_rename) <- c(paste0("Q", 1:(ncol(data_0_rename))), "label")

### Select items from Anxiety subscale
data1 <- data_0_rename %>%  # Anxiety subscale
  select(Q2,Q7,Q9,Q15,Q19, Q20, Q23, Q25, Q28, Q30, Q36, Q40, Q41)

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
summary(data0$gender) # Gender: "What is your gender?", 1=Male, 2=Female, 3=Other

# Age
summary(data0$age)

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
cut_off <- 2 #will be used with <= // Specify for individual analysis

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
write_csv2(data1_binarized, "Analysis/DASS/Generated_Data/DASS_binarized.csv")
write_csv2(data2_counted, "Analysis/DASS/Generated_Data/DASS_freq_count.csv")
write_csv2(datax, "Analysis/DASS/Generated_Data/DASS_Matched_freq_count.csv")


######  5. Session info #########################################################
sessionInfo()

#####################################  END  ####################################
