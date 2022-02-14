##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 4.0 (27.12.2021)                        #
#                                                                            #
#----------------------------------------------------------------------------#
#                                                                            #
#                            Script IV -  Figures                            #
#									                                                           #
##############################################################################


###### Table of Contents #####################################################
#----- 1. Load libraries ----------------------------------------------------#
#----- 2. Figure 1A ---------------------------------------------------------#
#----- 3. Figure 1B ---------------------------------------------------------#
#----- 4. Figure 1  ---------------------------------------------------------#
#----- 5. Session Info  -----------------------------------------------------#


## General Notes ##

###### 1. Load Libraries #####################################################
# Data handling + basic calculations
library("tidyverse")

# Power law
library("poweRlaw")

# Visualization
library("patchwork")
library("cowplot")
library("scales")

###### 2. FIGURE 1A ##############################################################
#### Import ####
## PANSS 
load("Analysis/PANSS/Generated Data/freq1_top_PANSS.RData") #ADJUST!!!
freq1_top_PANSS <- freq1_top_PANSS %>%   #ADJUST!!!
  top_n(n=50)                       

## PCL
load("Analysis/PCL5/Generated Data/freq1_top_PCL.RData") #ADJUST!!!
freq1_top_PCL <- freq1_top_PCL %>%  #ADJUST!!!
  top_n(n=50)

## DASS
load("Analysis/DASS/freq1_top_DASS.RData")
freq1_top_DASS <- freq1_top_DASS %>% 
  top_n(n=50)

## MBI
load("Analysis/MBI/Generated Data/freq1_top_MBI.RData")
freq1_top_MBI <- freq1_top_MBI %>% 
  top_n(n=50)

## PHQ
load("Analysis/PHQ9/Generated Data/freq1_top_PHQ9.RData") #ADJUST!!!
freq1_top_PHQ <- freq1_top_PHQ %>%  #ADJUST!!!
  top_n(n=50)


#### General Properties #### 
Theme_Figure_1a <- 
  theme(
    plot.title = element_text(size=12),
    axis.title.x = element_text(size=9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size=9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=9, color = "black", margin = margin(t = 0, r = 0, b = 0, l = 5)),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(size=.2, color="black" ), 
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA, size=1))

#### Individual Figures #### 
##PANSS
q1 <- ggplot(freq1_top_PANSS, aes(x=as.factor(1:nrow(freq1_top_MBI)),y=freq)) + #ADJUST!!!
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(" ") + 
  ylab("") +
  ggtitle("1. PANSS") +
  theme_minimal() + 
  Theme_Figure_1a

##PCL
q2 <- ggplot(freq1_top_PCL, aes(x=as.factor(1:nrow(freq1_top_DASS)),y=freq)) + #ADJUST!!!
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("") + 
  ylab("") +
  ggtitle("2. PCL")+
  theme_minimal() + 
  Theme_Figure_1a

##DASS
q3 <- ggplot(freq1_top_DASS, aes(x=as.factor(1:nrow(freq1_top_DASS)),y=freq)) + 
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("") + 
  ylab("Frequency") +
  ggtitle("3. DASS")+
  theme_minimal() + 
  Theme_Figure_1a


##MBI
q4 <- ggplot(freq1_top_MBI, aes(x=as.factor(1:nrow(freq1_top_MBI)),y=freq)) +
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(" ") + 
  ylab("") +
  ggtitle("4. MBI")+
  theme_minimal() + 
  Theme_Figure_1a

##PHQ
q5 <- ggplot(freq1_top_PHQ, aes(x=as.factor(1:nrow(freq1_top_MBI)),y=freq)) +
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("Phenotypes") + 
  ylab("") +
  ggtitle("5. PQH-9")+
  theme_minimal() + 
  Theme_Figure_1a



###### 3. FIGURE 1B ##############################################################
#### Import  #### 

## PANSS
load("Analysis/PANSS/Generated Data/res_pl_PANSS.RData") 
load("Analysis/PANSS/Generated Data/line_pl_PANSS.RData")
load("Analysis/PANSS/Generated Data/line_ln_PANSS.RData")
load("Analysis/PANSS/Generated Data/line_ex_PANSS.RData")

## PCL
load("Analysis/PCL5/Generated Data/res_pl_PCL.RData")
load("Analysis/PCL5/Generated Data/line_pl_PCL.RData")
load("Analysis/PCL5/Generated Data/line_ln_PCL.RData")
load("Analysis/PCL5/Generated Data/line_ex_PCL.RData")

## DASS
load("Analysis/DASS/res_pl_DASS.RData")
load("Analysis/DASS/line_pl_DASS.RData")
load("Analysis/DASS/line_ln_DASS.RData")
load("Analysis/DASS/line_ex_DASS.RData")

## MBI
load("Analysis/MBI/Generated Data/res_pl_MBI.RData")
load("Analysis/MBI/Generated Data/line_pl_MBI.RData")
load("Analysis/MBI/Generated Data/line_ln_MBI.RData")
load("Analysis/MBI/Generated Data/line_ex_MBI.RData")

## PHQ-9#
load("Analysis/PHQ9/Generated Data/res_pl_PHQ9.RData")
load("Analysis/PHQ9/Generated Data/line_pl_PHQ9.RData")
load("Analysis/PHQ9/Generated Data/line_ln_PHQ9.RData")
load("Analysis/PHQ9/Generated Data/line_ex_PHQ9.RData")

#### Individual Figures #### 
Theme_Figure_1b <- theme(
  plot.title = element_text(size=11),
  axis.title.x = element_text(size=9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
  axis.title.y = element_text(size=9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
  axis.text.x = element_text(size=9, color = "black"),
  axis.text.y = element_text(size=9, color = "black", margin = margin(t = 0, r = 0, b = 0, l = 5)),
  axis.ticks = element_blank(),
  panel.grid.major.x = element_line(size=.2, color="black"), 
  panel.grid.major.y = element_line(size=.2, color="black"), 
  panel.grid.minor.y = element_blank(), 
  panel.border = element_rect(colour = "black", fill=NA, size=1))


## PANSS
p1 <- ggplot(res_pl_PANSS, aes(x=x,y=y)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(10^-4, 1)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(1, 10^3)) + 
  geom_line(data = line_pl_PANSS, aes(x=x, y=y), color = "red", size = 1) +         
  geom_line(data = line_ln_PANSS, aes(x=x, y=y), color = "blue", size = 1,linetype = "dashed")+ 
  geom_line(data = line_ex_PANSS, aes(x=x, y=y), color = "orange", size = 1,linetype = "twodash")+ 
  geom_point(size = 1)+
  xlab("") + 
  ylab("") +
  ggtitle("")+
  theme_minimal_grid() +
  Theme_Figure_1b 

## PCL
p2 <- ggplot(res_pl_PCL, aes(x=x,y=y)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(10^-4, 1)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(1, 10^5)) + 
  geom_line(data = line_pl_PCL, aes(x=x, y=y), color = "red", size = 1) +        
  geom_line(data = line_ln_PCL, aes(x=x, y=y), color = "blue", size = 1,linetype = "dashed")+ 
  geom_line(data = line_ex_PCL, aes(x=x, y=y), color = "orange", size = 1,linetype = "twodash")+
  geom_point(size = 1)+
  ggtitle("")+
  xlab("") + 
  ylab("") +
  theme_minimal_grid() +
  Theme_Figure_1b 



## DASS
p3 <- ggplot(res_pl_DASS, aes(x=x,y=y)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(10^-4, 1)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(1, 10^4)) + 
  geom_line(data = line_pl_DASS, aes(x=x, y=y), color = "red", size = 1) +         
  geom_line(data = line_ln_DASS, aes(x=x, y=y), color = "blue", size = 1,linetype = "dashed")+ 
  geom_line(data = line_ex_DASS, aes(x=x, y=y), color = "orange", size = 1,linetype = "twodash")+ 
  geom_point(size = 1)+
  xlab("") + 
  ylab("CDF") +
  ggtitle("")+
  theme_minimal_grid() +
  Theme_Figure_1b 

## MBI
p4 <- ggplot(res_pl_MBI, aes(x=x,y=y)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(10^-4, 1)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(1, 10^4)) + 
  geom_line(data = line_pl_MBI, aes(x=x, y=y), color = "red", size = 1) +         
  geom_line(data = line_ln_MBI, aes(x=x, y=y), color = "blue", size = 1,linetype = "dashed")+ 
  geom_line(data = line_ex_MBI, aes(x=x, y=y), color = "orange", size = 1,linetype = "twodash")+
  geom_point(size = 1)+
  xlab("") + 
  ylab("") +
  ggtitle("")+
  theme_minimal_grid() +
  Theme_Figure_1b 

## PHQ
p5 <- ggplot(res_pl_PHQ, aes(x=x,y=y)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(10^-3, 1)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(1, 10^4)) + 
  geom_line(data = line_pl_PHQ, aes(x=x, y=y), color = "red", size = 1) +        
  geom_line(data = line_ln_PHQ, aes(x=x, y=y), color = "blue", size = 1,linetype = "dashed")+ 
  geom_line(data = line_ex_PHQ, aes(x=x, y=y), color = "orange", size = 1,linetype = "twodash")+
  geom_point(size = 1)+
  xlab("Frequency") + 
  ylab("") +
  ggtitle("")+
  theme_minimal_grid() +
  Theme_Figure_1b 


###### 4. FIGURE 1 FULL #########################################################

pdf("Figure_1.pdf", width=7.25, height=7.25) 
cowplot::plot_grid(
  q1, p1, q2, p2, q3, p3, q4, p4, q5, p5,
  labels=c("A", "B"),rel_widths = c(1, 2), 
  ncol = 2, nrow = 5)
dev.off()


######  7. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
