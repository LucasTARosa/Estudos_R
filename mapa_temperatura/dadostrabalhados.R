library(ggplot2)
library(geobr)
library(raster)
library(fields)
library(ggspatial)

setwd('C:/Users/Lucas/Videos/Estudos R/mapa_temperatura')
dados.temp <- read.csv('dados/dados_temperatura.csv')
relevo.mg <- raster('dados/relevo_minas_gerais.tif')
mg <- read_state(code_state = 'MG')


modelo <- lm(formula = temp~lon+lat+alt, data = dados.temp)

plot(relevo.mg)

relevo.df <- as.data.frame(relevo.mg, xy=TRUE)
relevo.df <- na.omit(relevo.df)
relevo.df

names(relevo.df) <- c('lon', 'lat', 'alt')
relevo.df

#duas formas de fazer, uma "manualmente" e outra com a função predict
relevo.df$temp <- 23.49-0.25*relevo.df$lon+0.48*relevo.df$lat-0.0053*relevo.df$alt

relevo.df$temp <- predict(modelo, relevo.df)

ggplot(relevo.df)+
  geom_raster(aes(x=lon, y=lat,fill=temp))+
  geom_sf(data = mg,fill='NA')+
  scale_fill_gradientn(colours = tim.colors(20))+
  annotation_scale()+
  annotation_north_arrow(location='tl', 
                         style= north_arrow_fancy_orienteering())+
  labs(x='',
       y='',
       title='Temperatura do ar no estado de Minas Gerais',
       fill='ºC')+
  theme_minimal()
 