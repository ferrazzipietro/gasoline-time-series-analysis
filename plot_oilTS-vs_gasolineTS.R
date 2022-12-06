data <- read.csv('data/merged_data.csv')
data$date <- as.Date(data$date)
plot(oil_price~date, data=data, main='Oil price vs gasoline price over time')
points( ((data$NETTO - 100) /10 )~date, data=data, col=2)
legend('topleft', legend=c('oil price', 'gasoline price'),
       col=1:2, pch=19)
