##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 6.1 (08.02.2023)                        #
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

###### 2. FIGURE 1A ##########################################################
#### Import ####
## PANSS 
load("Analysis/PANSS/Generated Data/freq1_top_PANSS.RData") 

## PCL
data2_counted_PCL <- read_delim("Analysis/PCL5/Generated Data/freq_count2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
load("Analysis/PCL5/Generated Data/freq1_top_PCL.RData") 

## DASS
load("Analysis/DASS/freq1_top_DASS.RData")

## MBI
load("Analysis/MBI/Generated Data/freq1_top_MBI.RData")

## PHQ
load("Analysis/PHQ9/Generated Data/freq1_top_PHQ9.RData")


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


Theme_Figure_1b <- 
  theme(
    plot.title = element_text(size=12),
    axis.title.x = element_text(size=9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size=9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=9, color = "black", margin = margin(t = 0, r = 0, b = 0, l = 5)),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(size=.2, color="black" ), 
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA, size=1))


###### 2. FIGURE 1A ##############################################################
### PCL
data2_counted_PCL <- data2_counted_PCL %>% 
  arrange(desc(freq)) %>% 
  select(-total_bin)

data2_counted_PCL_color_top <- data2_counted_PCL %>% 
  top_n(n=10) %>% 
  mutate(color = "#F97134",
         title = "A")

data2_counted_PCL_color_low <- data2_counted_PCL %>% 
  top_n(n= -(nrow(data2_counted_PCL)-10)) %>% 
  mutate(color = "#00A1D5FF",
         title = "B")

data2_counted_PCL_color <- rbind(data2_counted_PCL_color_top, data2_counted_PCL_color_low)

## Figure A - Overall
A1 <- ggplot(data2_counted_PCL_color, aes(x=as.factor(1:nrow(data2_counted_PCL_color)),y=freq)) +
  geom_bar(stat = "identity",fill = data2_counted_PCL_color$color) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("Frequency") + 
  ylab("") +
  ggtitle("PCL-5")+
  theme_minimal() + 
  Theme_Figure_1a

## Figure A -Distribution 
data2_counted_PCL_distribution <- matrix(nrow = 2,ncol = 3)
colnames(data2_counted_PCL_distribution) <- c("freq", "title", "color")

data2_counted_PCL_distribution[1,1] <- sum(data2_counted_PCL_color_top$freq)
data2_counted_PCL_distribution[2,1] <- sum(data2_counted_PCL_color_low$freq)

data2_counted_PCL_distribution[1,2] <- "A"
data2_counted_PCL_distribution[2,2] <- "B"

data2_counted_PCL_distribution[1,3] <- "#F97134"
  data2_counted_PCL_distribution[2,3] <- "#00A1D5FF"
    
data2_counted_PCL_distribution_df <- as.data.frame(data2_counted_PCL_distribution)
  
A1_distribution <- ggplot(data2_counted_PCL_distribution_df, aes(y=freq, x=title)) + 
    geom_bar(stat="identity", fill = data2_counted_PCL_distribution_df$color) +
    #ylim(0,52609) +
    xlab("") + 
    ylab(" ") +
    ggtitle("")+
    theme_minimal() + 
    Theme_Figure_1b

## Zoom
freq1_top_PCL <- freq1_top_PCL %>% 
  top_n(n=50)

freq1_top_PCL_color_top <- freq1_top_PCL %>% 
  top_n(n=10) %>% 
  mutate(color = "#F97134")

freq1_top_PCL_color_low <- freq1_top_PCL %>% 
  top_n(n=-40) %>% 
  mutate(color = "#00A1D5FF")

freq1_top_PCL_color <- rbind(freq1_top_PCL_color_top, freq1_top_PCL_color_low)

A1_zoom <- ggplot(freq1_top_PCL_color, aes(x=as.factor(1:nrow(freq1_top_PCL_color)),y=freq)) + 
    geom_bar(stat = "identity",fill = freq1_top_PCL_color$color) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    xlab("") + 
    ylab(" ") +
    ggtitle("")+
    theme_minimal() + 
    Theme_Figure_1b
  
  
  
###### 3. FIGURE 1B ##############################################################
  
##PANSS
freq1_top_PANSS <- freq1_top_PANSS %>% 
  top_n(n=50)

  freq1_top_PANSS_color_top <- freq1_top_PANSS %>% 
    top_n(n=9) %>% 
    mutate(color = "#F97134")
  
  freq1_top_PANSS_color_low <- freq1_top_PANSS %>% 
    top_n(n=-40) %>% 
    mutate(color = "#00A1D5FF")
  
  freq1_top_PANSS_color <- rbind(freq1_top_PANSS_color_top, freq1_top_PANSS_color_low)
  
B1 <- ggplot(freq1_top_PANSS, aes(x=as.factor(1:nrow(freq1_top_PANSS)),y=freq)) + #ADJUST!!!
    geom_bar(stat = "identity",fill = freq1_top_PANSS_color$color) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    xlab(" ") + 
    ylab("") +
    ggtitle("PANSS") +
    theme_minimal() + 
    Theme_Figure_1b
  
  
##PHQ
freq1_top_PHQ <- freq1_top_PHQ %>% 
  top_n(n=50)

  freq1_top_PHQ_color_top <- freq1_top_PHQ %>% 
    top_n(n=10) %>% 
    mutate(color = "#F97134")
  
  freq1_top_PHQ_color_low <- freq1_top_PHQ %>% 
    top_n(n=-40) %>% 
    mutate(color = "#00A1D5FF")
  
  freq1_top_PHQ_color <- rbind(freq1_top_PHQ_color_top, freq1_top_PHQ_color_low)
  
  B2 <- ggplot(freq1_top_PHQ_color, aes(x=as.factor(1:nrow(freq1_top_PHQ_color)),y=freq)) +
    geom_bar(stat = "identity",fill = freq1_top_PHQ_color$color) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    xlab("Phenotypes") + 
    ylab("") +
    ggtitle("PHQ-9")+
    theme_minimal() + 
    Theme_Figure_1b
  
  
##DASS
  freq1_top_DASS <- freq1_top_DASS %>% 
    top_n(n=50)
  
  freq1_top_DASS_color_top <- freq1_top_DASS %>% 
    top_n(n=10) %>% 
    mutate(color = "#F97134")
  
  freq1_top_DASS_color_low <- freq1_top_DASS %>% 
    top_n(n=-40) %>% 
    mutate(color = "#00A1D5FF")
  
  freq1_top_DASS_color <- rbind(freq1_top_DASS_color_top, freq1_top_DASS_color_low)
  
  
B3 <- ggplot(freq1_top_DASS_color, aes(x=as.factor(1:nrow(freq1_top_DASS_color)),y=freq)) + 
    geom_bar(stat = "identity",fill = freq1_top_DASS_color$color) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    xlab("") + 
    ylab("Frequency") +
    ggtitle("DASS")+
    theme_minimal() + 
    Theme_Figure_1b
  
  
##MBI
  freq1_top_MBI <- freq1_top_MBI %>% 
    top_n(n=50)
  
  freq1_top_MBI_color_top <- freq1_top_MBI %>% 
    top_n(n=10) %>% 
    mutate(color = "#F97134")
  
  freq1_top_MBI_color_low <- freq1_top_MBI %>% 
    top_n(n=-40) %>% 
    mutate(color = "#00A1D5FF")
  
  freq1_top_MBI_color <- rbind(freq1_top_MBI_color_top, freq1_top_MBI_color_low)
  
  
B4 <- ggplot(freq1_top_MBI_color, aes(x=as.factor(1:nrow(freq1_top_MBI_color)),y=freq)) +
    geom_bar(stat = "identity",fill = freq1_top_MBI_color$color) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    xlab(" ") + 
    ylab("") +
    ggtitle("MBI-GS")+
    theme_minimal() + 
    Theme_Figure_1b
  
  
###### 4. FIGURE 1 FULL #########################################################
# Build the top row
top_row <- ggdraw(A1) +
    draw_plot(A1_zoom, .1, .4, .3, .5) +
    draw_plot(A1_distribution, .5, .4, .3, .5)
  
  
# Build the bottom row
bottom_row <- plot_grid(B1, B2, B3,
                          ncol = 3, nrow = 1)
  
# Build the full plot
pdf("Figure_1.pdf", width=10, height=7.25) 
cowplot::plot_grid(
    top_row, bottom_row,
    label_size = 14,
    labels=c("A", "B"),rel_widths = c(1, 1), 
    ncol = 1, nrow = 2)
dev.off()
  
  
  
######  5. Session info #########################################################
sessionInfo()
  
  
  
######################################  END  ####################################
