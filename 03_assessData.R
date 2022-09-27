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
  geom_hline(yintercept=50, col= 'red', linetype= 'dashed')
  
## get only native vegetation
native <- subset(recipe, class == 'Vegetação Nativa')

## compuite perc
native$perc <- round(native$area/sum(native$area) * 100, digits= 2)

## plot
ggplot(data= native, mapping= aes(x= reorder(tenure, area), y= area/1e6)) +
  geom_bar(stat='identity', fill= '#129912', alpha= 0.8) +
  geom_text(mapping=aes(label= paste0(round(area/1e6, digits=2), 'Mha - ', perc, '%')), hjust=-0.07) +
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
# ggplot(data= loss, mapping= aes(x=area/1e6, y= loss/1e6)) +
#   geom_text_repel(mapping= aes(label=tenure), size=3) +
#   geom_point(mapping=aes(size= rel_loss, col= tenure),alpha=0.7) + 
#   theme_bw() +
#   scale_y_log10() +
#   #scale_x_log10() +
#   xlab('Vegetação nativa remanescente (Mha)') +
#   ylab('Perda liíquida de vegetação nativa (1985 - 2021, Mha)')

## zoom to IR
ir <- subset(data, tenure_l1 == 'IR')

## organize array by property size
ir$tenure_ir <- 
  gsub('SIGEF_SNCI Privado_Pequeno', 'Pequena (<4 MF)',
     gsub('CAR_Pequeno', 'Pequena (<4 MF)',
          gsub('SIGEF_SNCI Privado_Medio', 'Média (>4 & <15 MF)',
               gsub('CAR_médio', 'Média (>4 & <15 MF)',
                    gsub('SIGEF_SNCI Privado_Grande', 'Grande (>15 MF)',
                         gsub('CAR_grande', 'Grande (>15 MF)',
                              ir$tenure_l2))))))

## get only 2021
ir21 <- subset(ir, year == 2021)

## aggregate native vegetation vs. anthropic
ir21 <- aggregate(x=list(area= ir21$area), by=list(size= ir21$tenure_ir), FUN= 'sum')
## get perc
ir21$perc <- round(ir21$area/sum(ir21$area) * 100, digits=1)

## plot size of each IR class size
ggplot(ir21, mapping= aes(area= area, fill= size,
                          label=paste0(size, '\n', perc, '% (', round(area/1e6, digits=1), ' Mha)'))) +
  geom_treemap(alpha=1, col='gray20') +
  scale_fill_manual('Tamanho da propriedade', values=c('#59B0B4', '#F0C585', '#F384DA')) + 
  geom_treemap_text(size=13) 


## get only native
ir21 <- subset(ir, year == 2021 & mapb_0 == 'Vegetação Nativa')

## aggregate
ir21 <- aggregate(x=list(area= ir21$area), by=list(size= ir21$tenure_ir), FUN= 'sum')
## calc perc
ir21$perc <- round(ir21$area/sum(ir21$area) * 100, digits=1)

## plot native vegetation
ggplot(data= ir21, mapping= aes(x= reorder(size, area), y= area/1e6)) +
  geom_bar(stat='identity', fill= '#129912', alpha= 0.8) +
  geom_text(mapping=aes(label= paste0(round(area/1e6, digits=2), 'Mha - ', perc, '%')), hjust=1) +
  coord_flip() +
  theme_minimal() +
  xlab(NULL) +
  ylab('Área (Mha)')

## compute native vs anthropic
ir21 <- subset(ir, year == 2021 & mapb_0 != 'Não aplica')

## aggregate
ir21 <- aggregate(x=list(area= ir21$area), by=list(size= ir21$tenure_ir, class= ir21$mapb_0), FUN= 'sum')

## compute proportions
recipe <- as.data.frame(NULL)
for (i in 1:length(unique(ir21$size))) {
  ## get size i
  x <- subset(ir21, size == unique(ir21$size)[i])
  ## compute proportion
  x$perc <- round(x$area/sum(x$area) * 100, digits=1)
  ## bind
  recipe <- rbind(x, recipe); rm(x)
}

## plot proportions








## select only native vegetation
ir_n <- subset(ir, mapb_0 == 'Vegetação Nativa')

## aggregate 
ir_n0 <- aggregate(x= list(area= ir_n$area), by= list(size= ir_n$tenure_ir, year= ir_n$year), FUN= 'sum')

## compute loss by year for each class size
recipe <- as.data.frame(NULL)
for (i in 1:length(unique(ir_n0$size))) {
  ## subset class size
  x <- subset(ir_n0, size == unique(ir_n0$size)[i])
  ## create xij to receive data
  xij <- as.data.frame(NULL)
  ## compute loss over years
  for (j in 1:length(unique(ir_n0$year))) {
    ## if no previous year exists, put 0 
    if (unique(ir_n0$year)[j] == 1985) {
      loss <- 0
      relative_loss <- 0
    }
    
    ## if previous year exists, compute loss
    if (unique(ir_n0$year)[j] != 1985) {
      ## get previous year
      yi <- subset(x, year == unique(ir_n0$year)[j-1])
      ## get current
      yij <- subset(x, year == unique(ir_n0$year)[j])
      ## get loss
      loss <- yi$area - yij$area
      ## get relative loss
      relative_loss <- loss / yi$area * 100
      
    }
    
    ## insert loss and paste metadata as data.frame
    r <- as.data.frame(cbind(size = unique(x$size),
                             year = unique(ir_n0$year)[j],
                             loss = loss,
                             relative_loss = relative_loss))
    
    ## bind into recipe
    xij <- rbind(xij, r)
  }
  
  ## compute cummulative sum
  xij$cumsum <- cumsum(xij$loss)
  xij$cumsum_relative <- cumsum(xij$relative_loss)
  
  
  ## insert into recipe
  recipe <- rbind(xij, recipe)
}

rm(x, loss, yi, yij, xij, r, relative_loss)

## plot cummulative (relative)
ggplot(data= recipe, mapping= aes(x= as.numeric(year), y= cumsum_relative, colour= size)) +
  geom_point(size=2, mapping=aes(shape= size), alpha= 0.8) +
  geom_smooth(method= 'loess', se= FALSE) +
  #geom_line(alpha=0.2, size=4) +
  scale_colour_manual('Tamanho da propriedade', values=c('red', 'orange', 'purple')) + 
  theme_minimal() +
  xlab(NULL) +
  ylab('Perda líquida relativa acumulada (%)')


## plot cummulative (absolute)
ggplot(data= recipe, mapping= aes(x= as.numeric(year), y= cumsum/1e6, colour= size)) +
  geom_point(size=2, mapping=aes(shape= size), alpha= 0.8) +
  geom_smooth(method= 'loess', se= FALSE) +
  #geom_line(alpha=0.2, size=4) +
  scale_colour_manual('Tamanho da propriedade', values=c('red', 'orange', 'purple')) + 
  theme_minimal() +
  xlab(NULL) +
  ylab('Perda líquida absoluta acumulada (Mha)')















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



