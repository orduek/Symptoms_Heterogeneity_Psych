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
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("readxl")) install.packages("readxl")
if(!require("foreign")) install.packages("foreign")
if(!require("ReIns")) install.packages("ReIns")
if(!require("psych")) install.packages("psych")

###### 2. Import and prepare data ############################################
#### Import
#### Import
data <- read_delim("Raw Data/DASS_data_21.02.19/data.csv", escape_double = FALSE, trim_ws = TRUE)

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
write_csv2(data1_binarized, "Generated_Data/DASS_binarized.csv")
write_csv2(data2_counted, "Generated_Data/DASS_freq_count.csv")
write_csv2(datax, "Generated_Data/DASS_Matched_freq_count.csv")


######  5. Session info #########################################################
sessionInfo()

# R version 4.0.2 (2020-06-22)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: OS X Snow Leopard 11.6
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] de_CH.UTF-8/de_CH.UTF-8/de_CH.UTF-8/C/de_CH.UTF-8/de_CH.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] psych_2.1.6     ReIns_1.0.10    foreign_0.8-81  readxl_1.3.1    forcats_0.5.1   stringr_1.4.0   dplyr_1.0.7     purrr_0.3.4     readr_2.0.1     tidyr_1.1.3     tibble_3.1.4   
# [12] ggplot2_3.3.5   tidyverse_1.3.1
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.7        lubridate_1.7.10  lattice_0.20-44   foreach_1.5.1     assertthat_0.2.1  utf8_1.2.2        R6_2.5.1          cellranger_1.1.0  plyr_1.8.6        backports_1.2.1  
# [11] reprex_2.0.1      pracma_2.3.3      httr_1.4.2        pillar_1.6.2      rlang_0.4.11      rstudioapi_0.13   Matrix_1.3-4      splines_4.0.2     bit_4.0.4         munsell_0.5.0    
# [21] broom_0.7.9       compiler_4.0.2    modelr_0.1.8      pkgconfig_2.0.3   mnormt_2.0.2      tmvnsim_1.0-2     tidyselect_1.1.1  codetools_0.2-18  fansi_0.5.0       crayon_1.4.1     
# [31] tzdb_0.1.2        dbplyr_2.1.1      withr_2.4.2       grid_4.0.2        nlme_3.1-152      jsonlite_1.7.2    gtable_0.3.0      lifecycle_1.0.0   DBI_1.1.1         magrittr_2.0.1   
# [41] scales_1.1.1      vroom_1.5.4       cli_3.0.1         stringi_1.7.4     fs_1.5.0          doParallel_1.0.16 xml2_1.3.2        ellipsis_0.3.2    generics_0.1.0    vctrs_0.3.8      
# [51] iterators_1.0.13  tools_4.0.2       bit64_4.0.5       glue_1.4.2        poweRlaw_0.70.6   hms_1.1.0         parallel_4.0.2    survival_3.2-13   colorspace_2.0-2  rvest_1.0.1      
# [61] haven_2.4.3

#####################################  END  ####################################
