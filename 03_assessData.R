## assess tenure and cover data
## dhemerson.costa@ipam.org.br

## get libraries
library (ggplot2)
library (treemapify)
library(ggrepel)

## avoid scientific notation
options(scipen=999)

## read data
data <- read.csv('./table/lcluc-tenure-state/pre-process/cover-per-tenure-per-state.csv')

## get only 2021
x <- subset(data, year == 2021)

## aggregate statistics
y <- aggregate(x= list(area= x$area), by=list(tenure= x$tenure_l1, class= x$mapb_0), FUN= 'sum')

## aggregate again (overview of cerrado)
y2 <- subset(y, class != 'Não aplica')
y2 <- aggregate(x=list(area= y2$area), by= list(tenure= y2$tenure), FUN='sum')

## compute percents
y2$perc <- round(y2$area/sum(y2$area) * 100, digits=1)

## plot land tenure in 2021
ggplot(y2, mapping= aes(area= area, fill= tenure, 
                        label= paste0(tenure, '\n', perc, '% (', round(area/1e6, digits=1),' Mha)'))) +
  geom_treemap(alpha=0.9, col='gray20') +
  scale_fill_manual(NULL, values=c('#F0FE0C', '#65923D', '#4FF6E7', '#FD60DE', '#FFDF83', '#AEA985',
                                   '#999892', '#FA4C2C', '#3F8D80', '#3BDA22')) +
  geom_treemap_text(size=15) 
  
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
  coord_flip() +
  geom_hline(yintercept=53.2, col= 'red', linetype= 'dashed')
  
## get only native vegetation
native <- subset(recipe, class == 'Vegetação Nativa')

## compuite perc
native$perc <- round(native$area/sum(native$area) * 100, digits= 2)

## plot
ggplot(data= native, mapping= aes(x= reorder(tenure, area), y= area/1e6)) +
  geom_bar(stat='identity', fill= '#129912', alpha= 0.8) +
  geom_text(mapping=aes(label= paste0(round(area/1e6, digits=2), 'Mha - ', round(area/1e6, digits=1), '%')), hjust=-0.07) +
  theme_minimal() +
  xlab(NULL) +
  ylab('Área (Mha)') +
  coord_flip()

## compute loss of native vegetation
## get 1985 and 2021 data
x <- subset(data, year == 1985 & mapb_0 == 'Vegetação Nativa' |year == 2021 & mapb_0 == 'Vegetação Nativa')
## aggregate per year
y <- aggregate(x=list(area= x$area), by= list(class= x$mapb_0, year= x$year, tenure= x$tenure_l1), FUN='sum')
## compute loss
loss <- as.data.frame(NULL)
for (i in 1:length(unique(y$tenure))) {
  ## subset
  z <- subset(y, tenure == unique(y$tenure)[i])
  ## compute loss
  z$loss <- z$area[1] - z$area[2]
  ## get relative loss (cmparting total to 1985)
  z$rel_loss <- round(z$loss/z$area * 100, digits=1)[1]
  z <- z[2,]
  ## bind
  loss <- rbind(z, loss); rm(z)
}

## compuite perc in relation to the total
loss$perc_tot <- round(loss$loss/sum(loss$loss) * 100, digits=2)

## plot loss
ggplot(data= loss, mapping= aes(x= reorder(tenure, area), y= loss/1e6)) +
  geom_bar(stat='identity', fill= 'tomato1', alpha=0.8) +
  geom_text_repel(mapping=aes(label= paste0(round(loss/1e6, digits=2), 'Mha - ', perc_tot, '%'))) +
  coord_flip() +
  theme_minimal() +
  xlab(NULL) +
  ylab('Área (Mha)')


## plot relation among tenure and loss
ggplot(data= loss, mapping= aes(x=area/1e6, y= loss/1e6)) +
  geom_text_repel(mapping= aes(label=tenure), size=3) +
  geom_point(mapping=aes(size= rel_loss, col= tenure),alpha=0.7) + 
  theme_bw() +
  scale_y_log10() +
  #scale_x_log10() +
  xlab('Vegetação nativa remanescente (Mha)') +
  ylab('Perda liíquida de vegetação nativa (1985 - 2021, Mha)')

## get native vegetation
per_class <- subset(data, mapb_1_2 == 'Formação Florestal' | mapb_1_2 == 'Formação Savânica' |
                      mapb_1_2 == 'Formação Campestre' | mapb_1_2 == 'Campo Alagado e Área Pantanosa')

## aggregate
per_class <- aggregate(x=list(area= per_class$area), 
                       by= list(class= per_class$mapb_1_2, tenure= per_class$tenure_l1, year= per_class$year),
                       FUN='sum')

## get 2021
x21 <- subset(per_class, year == 2021)

## plot distributions
ggplot(data= x21, mapping= aes(x= reorder(tenure, area), y= area/1e6, fill= class)) +
  geom_bar(stat= 'identity') +
  scale_fill_manual(values=c("#45C2A5", "#B8AF4F", "#006400", "#32CD32")) +
  theme_minimal() +
  coord_flip() +
  ylab('Área (Mha)') +
  xlab(NULL)

## compuite percents per class
y21 <- as.data.frame(NULL)
for (i in 1:length(unique(x21$class))) {
  ## get class
  z <- subset(x21, class == unique(x21$class)[i])
  ## compute percent
  z$perc <- round(z$area/sum(z$area) * 100, digits=1)
  ## bind
  y21 <- rbind(y21, z); rm(z)
}

## ggtree
ggplot(y21, mapping= aes(area = area/1e6, fill = tenure, 
                         label= paste0(tenure, '\n', perc,'% (', round(area/1e6, digits=1), 'Mha)'))) +
  geom_treemap(alpha=0.9, col= 'white') +
  scale_fill_manual(NULL, values=c('#F0FE0C', '#65923D', '#4FF6E7', '#FD60DE', '#FFDF83', '#AEA985',
                                   '#999892', '#FA4C2C', '#3F8D80', '#3BDA22')) +
  geom_treemap_text(size=15) +
  facet_wrap(~class) +
  theme_bw()



