library(ggpubr)
library(ggplot2)
library(GGally)
library(dplyr)
library(reshape)
library(rlist)

data <- read.csv('data/merged_data.csv') %>% as_tibble
vars <- (data %>% colnames)[c(10,11,12,13,14)]
n <- nrow(data)
objective_variable = 'PRICE'
#objective_variable = 'NET.PRICE'

######## EXPLANATORY ANALISYS ###########
g <- ggplot(data, aes(y='PRICE'))

#!!! THIS DOESN'T TAKE INTO ACCOUNT THE TIME CORRELATION !!!
mycolors <- c("corr"="royalblue", "Corr"="lightblue")
ggpairs(data %>% select(all_of(vars)),
        aes(col=rep("corr",n),
            fill=rep("Corr",n),
            alpha = 0.4))  +
  scale_fill_manual(values = mycolors, 
                    aesthetics = c("colour", "fill")) + 
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
#

data_scaled <- data %>% select(all_of(vars)) %>%
  scale() %>% as_tibble() %>% mutate(X=c(1:n)) %>% as.data.frame()
df.melted <- melt(data_scaled, id = 'X')
ggplot(data = df.melted, aes(x = X, y = value, color = variable)) +
  geom_line(size=0.9, linetype=1) +  
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())

pairs_plot <- list()
for (var in vars){
  data_plot <- data %>% 
    select(all_of(objective_variable), all_of(var)) %>%
    scale() %>% as.data.frame() %>% 
    mutate(time = c(1:n)) 
  df.melted <- melt(data_plot, id = 'time')
  pair_plot <- ggplot(data = df.melted, aes(x = time, y = value, color = variable)) +  
    geom_line() +
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank()) 
  pairs_plot <- pairs_plot %>% list.append(pair_plot)
}
ggarrange(ggarrange(pairs_plot[[1]],pairs_plot[[2]], pairs_plot[[3]], pairs_plot[[4]],pairs_plot[[5]],
                    ncol=3, nrow=2) ) %>% 
  annotate_figure(top=text_grob("TIME",face="bold"))

