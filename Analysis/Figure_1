---
title: "Figure 1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r Load Libraries, message=FALSE, warning=TRUE, include=FALSE}
# Data handling
library(tidyverse)
library(poweRlaw)
library(cowplot)
library(scales)
```

## Plots

```{r pressure, echo=FALSE}
x <- seq(1, 100, length.out=10000)
data_exp <- data.frame(x=x, px=dexp(x, rate=0.33)) %>% 
  add_column(group = 1)

data_log <- data.frame(x=x, px=dlnorm(x, meanlog = 0, sdlog = 1)) %>% 
  add_column(group = 2)

data_pwr <- data.frame(x=x, px=dplcon(x, xmin = 1, alpha=2)) %>% 
  add_column(group = 3)
  

data_dist <- rbind(data_exp, data_log, data_pwr)

data_dist$group <- as.factor(data_dist$group)

```

```{r Plot, echo=FALSE}
p1 <- ggplot(data_dist, aes(x=x, y = px)) +
  geom_line(data = data_dist , aes(x = x, y = px, 
                                            group = group,
                                            color=group)) +
scale_y_continuous(#expand = c(0,0),
                     limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(limits = c(1, 10),
    breaks = c(1,3,5,7,9)) +
  ylab("y") +
  xlab("x") +
  theme_cowplot() +
  scale_color_manual(values=c("orange", "blue", "red"),
                     labels = c("Exponential", "Log-normal", "Power-law")) +
  theme(
    plot.title = element_text(size=11),
    axis.title.x = element_text(size=11, 
                                margin = margin(t = 5, r = 0, b = 0, l = 0), 
                                colour = "black"),
    axis.title.y = element_text(size=11, 
                                margin = margin(t = 0, r = 5, b = 0, l = 0), 
                                colour = "black"),
    axis.text.x = element_text(size=11, color = "black"),
    axis.text.y = element_text(size=11, color = "black", 
                               margin = margin(t = 0, r = 2, b = 0, l = 0)),
    axis.ticks = element_line(colour = "black", size=0.4),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(),
    legend.position = c(.70, .85),
    legend.title = element_blank())

p1
```
```{r Plot, echo=FALSE}
p2 <- ggplot(data_dist, aes(x=x, y = px)) +
  geom_line(data = data_dist , aes(x = x, y = px, 
                                            group = group,
                                            color=group)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(10^-6, 1)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0, 0),
                limits = c(1, 10^2)) + 
  ylab("log(y)") +
  xlab("log(x)") +
  theme_cowplot() +
  scale_color_manual(values=c("orange", "blue", "red"),
                     labels = c("Exponential", "Log-normal", "Power-law")) +
  theme(
    plot.title = element_text(size=11),
    axis.title.x = element_text(size=11, 
                                margin = margin(t = 5, r = 0, b = 0, l = 0), 
                                colour = "black"),
    axis.title.y = element_text(size=11, 
                                margin = margin(t = 0, r = 5, b = 0, l = 0), 
                                colour = "black"),
    axis.text.x = element_text(size=11, color = "black"),
    axis.text.y = element_text(size=11, color = "black", 
                               margin = margin(t = 0, r = 2, b = 0, l = 0)),
    axis.ticks = element_line(colour = "black", size=0.4),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(),
    legend.position = c(.70, .85),
    legend.title = element_blank())

p2
```

```{r Plot, echo=FALSE}
pdf("Figure_1.pdf", width=10, height=5)
cowplot::plot_grid(p1,p2,
  labels=c("A", "B"),rel_widths = c(1, 1), 
  ncol = 2, nrow = 1)
dev.off()
```
