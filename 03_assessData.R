## assess tenure and cover data
## dhemerson.costa@ipam.org.br

## get libraries
library (ggplot2)

## avoid scientific notation
options(scipen=999)

## read data
data <- read.csv('./table/lcluc-tenure-state/pre-process/cover-per-tenure-per-state.csv')

## get only 2021
x <- subset(data, year == 2021)

## aggregate statistics
y <- aggregate(x= list(area= x$area), by=list(tenure= x$tenure_l1, class= x$mapb_0), FUN= 'sum')

## plot
ggplot(data= subset(y, class != "Não aplica"),
       mapping=aes(x= reorder(tenure, area), y= area/1e6, fill= class)) +
  geom_bar(stat='identity') +
  scale_fill_manual(NULL, values=c('orange', '#129912')) +
  theme_minimal() +
  xlab(NULL) +
  ylab('Área (Mha)') +
  coord_flip()

## compute proportions
recipe <- as.data.frame(NULL)
for (i in 1:length(unique(y$tenure))) {
  ## get data
  z <- subset(subset(y, tenure == unique(y$tenure)[i]), class != 'Não aplica')
  ## compute percents
  z$perc <- round(z$area/sum(z$area) * 100, digits=1)
  ## bind
  recipe <- rbind(recipe, z); rm(z)
}

## plot percents
ggplot(data= recipe, 
       mapping= aes(x= reorder(tenure, area), y= perc, fill= class)) +
  geom_bar(stat='identity', alpha= 0.8) +
  geom_text(mapping=aes(label= paste0(perc, '%')), position = position_stack(vjust = .5)) + 
  scale_fill_manual(NULL, values=c('orange', '#129912')) +
  theme_minimal() +
  xlab(NULL) +
  ylab('Porcentagem %') +
  coord_flip()
  
  
