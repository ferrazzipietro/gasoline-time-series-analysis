suppressMessages(library(ggpubr))
suppressMessages(library(ggplot2))
suppressMessages(library(GGally))
suppressMessages(library(dplyr))
suppressMessages(library(reshape))
suppressMessages(library(rlist))
options(warn=-1)

plot_each_single_ts <- function(data, variables_names, my_theme, col='darkblue', linewidth=3){
  gg <- ggplot(data, aes(x=date))
  for (var in variables_names){
    png(paste('plots/variables_ts/', var, '_ts_plot.png',sep=''),  width = 1200, heigh = 750)
    plot <- gg + 
      geom_line(aes_string(y=var), col=col, linewidth=linewidth) +
      labs(x = "Time", y = var) +
      my_theme
    print(plot)
    dev.off()
  }
}

plot_variables_vs_price <- function(data, variables_names, my_theme){
  g <- ggplot(data, aes(col=Event))
  for (var in variables_names){
     data_plot <- data %>% 
      select(all_of(objective_variable), all_of(var)) %>%
      scale() %>% as.data.frame() %>% 
      mutate(date=data$date)
    df.melted <- melt(data_plot, id = 'date')   %>% as_tibble 
    df.melted$variable <- forcats::fct_rev(df.melted$variable)
    
    png(paste('plots/variables_vs_price/', 'price_vs_', var, '.png',sep=''),  width = 1200, heigh = 750)
    plot <- ggplot(data = df.melted, aes(x = date, y = value, color = variable, 
                                         linetype = variable)) +  
      geom_line(linewidth=3)  +
      scale_fill_manual(values = c('blue4','blue'), 
                        aesthetics = c("colour"),
                        labels=c(var, 'gasoline price')) +
      my_theme +
      theme(axis.title.y=element_blank(), 
            axis.text.y=element_blank(),
            legend.text=element_text(size=30),
            legend.title=element_text(size=35))+ 
      guides(linetype="none")
    print(plot)
    dev.off()
  }
}

## USELESS FUNCTIION ##
explainatory_vars_vs_themselves <- function(){
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
}

set_my_theme <- function(){
  my_theme <- theme(axis.title = element_text(face='bold', color='darkblue', size = 35),
                    axis.text = element_text( color='darkblue', size = 20),
                    panel.background = element_blank(),
                    panel.grid = element_line(linetype = 2, colour = 'darkblue', linewidth=0.3))
  return(my_theme)
}

main <- function(){
  data <- read.csv('data/merged_data_NAs.csv') %>% as_tibble %>% 
    mutate(date=as.Date(date))
  vars <- (data %>% colnames)[c(8:12)]
  n <- nrow(data)
  objective_variable = 'PRICE'
  
  my_theme <- set_my_theme()
  
  plot_each_single_ts(data, c(vars, objective_variable), my_theme )
  plot_each_single_ts(data, vars, my_theme)
}

main()

