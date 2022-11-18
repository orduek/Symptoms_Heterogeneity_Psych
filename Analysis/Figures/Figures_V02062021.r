##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 5.0 (18.11.2022)                        #
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
## PCL
load("Analysis/PCL5/Generated Data/freq1_top_PCL.RData") 
freq1_top_PCL <- freq1_top_PCL %>%  
  top_n(n=50)

## VA 
load("Analysis/PANSS/Generated Data/freq1_top_VA.RData") 
freq1_top_VA <- freq1_top_VA %>%
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
##PCL- Screening
q1 <- ggplot(freq1_top_PCL, aes(x=as.factor(1:nrow(freq1_top_VA)),y=freq)) 
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("") + 
  ylab("") +
  ggtitle( "1. PCL - Screening")+
  theme_minimal() + 
  Theme_Figure_1a

##PCL -RPTP
q2 <- ggplot(freq1_top_VA, aes(x=as.factor(1:nrow(freq1_top_VA)),y=freq))
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(" ") + 
  ylab("") +
  ggtitle("2. PCL - RPTP") +
  theme_minimal() + 
  Theme_Figure_1a




###### 3. FIGURE 1B ##############################################################
#### Import  #### 

## PCL
load("Analysis/PCL5/Generated Data/res_pl_PCL.RData")
load("Analysis/PCL5/Generated Data/line_pl_PCL.RData")
load("Analysis/PCL5/Generated Data/line_ln_PCL.RData")
load("Analysis/PCL5/Generated Data/line_ex_PCL.RData")

## DASS
load("Analysis/VA/Generated Data/res_pl_VA.RData")
load("Analysis/VA/Generated Data/line_pl_VA.RData")
load("Analysis/VA/Generated Data/line_ln_VA.RData")
load("Analysis/VA/Generated Data/line_ex_VA.RData")

#### Individual Figures #### 

# define transperancy of points
alpha = .4

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


## PCL - Screening
p1 <- ggplot(res_pl_PCL, aes(x=x,y=y)) +
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
  geom_point(size = 1, alpha=alpha)+
  ggtitle("")+
  xlab("") + 
  ylab("") +
  theme_minimal_grid() +
  Theme_Figure_1b 

## PCL - VA
p2 <- ggplot(res_pl_VA, aes(x=x,y=y)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(10^-4, 1)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(1, 10^5)) + 
  geom_line(data = line_pl_VA, aes(x=x, y=y), color = "red", size = 1) +         
  geom_line(data = line_ln_VA, aes(x=x, y=y), color = "blue", size = 1,linetype = "dashed")+ 
  geom_line(data = line_ex_VA, aes(x=x, y=y), color = "orange", size = 1,linetype = "twodash")+ 
  geom_point(size = 1, alpha=alpha)+
  xlab("") + 
  ylab("") +
  ggtitle("")+
  theme_minimal_grid() +
  Theme_Figure_1b 


###### 4. FIGURE 1 FULL #########################################################

pdf("Figure_1.pdf", width=7.25, height=7.25) 
cowplot::plot_grid(
  q1, p1, q2, p2,
  labels=c("A", "B"),rel_widths = c(1, 2), 
  ncol = 2, nrow = 2)
dev.off()


######  7. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
