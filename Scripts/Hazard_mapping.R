# Insumos -----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(sf)
library(tmap)
library(extrafont)
library(viridis)
library(forcats)

# Lectura de datos --------------------------------------------------------
read_csv('../Datos/Datos-Historicos_v1.csv')%>% 
  as_tibble() %>% 
  mutate(Evento       = as.factor(Evento),
         Departamento = as.factor(Departamento),
         Municipio    = as.factor(Municipio)) %>% 
  mutate(Afectados    = if_else(is.na(Afectados), 0, Afectados)) %>% 
  mutate(Damnificados = if_else(is.na(Damnificados), 0, Damnificados)) -> data 


# Remover columnas
rmcol <- c('ACCIDENT', 'POLLUTION', 'BIOLOGICAL',
           'COASTLINE', 'EPIDEMIC', 'PLAGUE',
           'STRUCTURE', 'SURGE', 'LEAK', 'EXPLOSION',
           'SEDIMENTATION', 'FIRE')

# Nueva tabla de datos
data %>% 
  select(Departamento,Evento,Afectados,Damnificados) %>% 
  filter(!Evento %in% rmcol) -> newdata


# Cambiando nombres de los eventos

newdata <- newdata %>% 
  mutate(Evento = recode(Evento,'FLOOD'='Inundaciones')) %>% 
  mutate(Evento = recode(Evento,'LANDSLIDE'='Deslizamientos')) %>% 
  mutate(Evento = recode(Evento,'STRONGWIND'='Vientos fuertes')) %>% 
  mutate(Evento = recode(Evento,'RAIN'='Lluvias')) %>% 
  mutate(Evento = recode(Evento,'SPATE'='Avalancha')) %>% 
  mutate(Evento = recode(Evento,'STORM'='Tormentas')) %>% 
  mutate(Evento = recode(Evento,'FORESTFIRE'='Incendios forestales')) %>% 
  mutate(Evento = recode(Evento,'ERUPTION'='Erupciones volcánicas')) %>% 
  mutate(Evento = recode(Evento,'HAILSTORM'='Granizada')) %>% 
  mutate(Evento = recode(Evento,'EARTHQUAKE'='Terremotos')) %>% 
  mutate(Evento = recode(Evento,'DROUGHT' = 'Sequías')) %>% 
  mutate(Evento = recode(Evento,'FROST'='Escarcha')) %>%
  mutate(Evento = recode(Evento,'ELECTRICSTORM'='Tormenta eléctrica')) %>% 
  mutate(Evento = recode(Evento,'HURRICANE'='Huracán'))



# Gráficos :: Eventos por afectados  --------------------------------------
options(scipen=10000)

newdata %>% 
  group_by(Evento) %>%
  summarise(Total = sum(Afectados)) %>% 
  ggplot(aes(x = reorder(Evento,Total), 
             y = Total,
             fill = Total)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  labs(y = 'Número de afectados',
       x= '') +
  scale_y_discrete(limits = seq(0,3500000,length.out = 5),
                   labels = c("0","875k","1750k","2625k","3500k")) + 
  scale_fill_viridis() +
  theme_light() + 
  theme(legend.text = element_text(family  = 'DejaVu Sans Mono',
                                   face    = 'bold', 
                                   color   = 'black'),
        axis.text.y = element_text(family  = 'DejaVu Sans Mono',
                                   face    = 'bold',
                                   color   = 'black'),
        axis.text.x = element_text(family  = 'DejaVu Sans Mono', 
                                   face    = 'bold', 
                                   color   = 'black'),
        axis.title.x = element_text(family = 'DejaVu Sans Mono', 
                                    face   = 'bold', 
                                    color  = 'black'), 
        legend.position = 'none') 

ggsave(filename = '../Gráficos/EventosxAfectados.png',
       plot = last_plot(),width = 12,height = 6)


# Gráficos :: Damnificados ------------------------------------------------

newdata %>% 
  group_by(Evento) %>%
  summarise(Total = sum(Damnificados)) %>% 
  arrange(desc(Total)) -> vista

newdata %>% 
  group_by(Evento) %>%
  summarise(Total = sum(Damnificados)) %>% 
  ggplot(aes(x = reorder(Evento,Total), 
             y = Total,
             fill = Total)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  labs(y = 'Número de Damnificados',
       x= '') +
  scale_y_discrete(limits = seq(0,600000,length.out = 4),
                   labels = c('0', '200k', '400k', '600k')) + 
  scale_fill_viridis() +
  theme_light() + 
  theme(legend.text = element_text(family  = 'DejaVu Sans Mono',
                                   face    = 'bold', 
                                   color   = 'black'),
        axis.text.y = element_text(family  = 'DejaVu Sans Mono',
                                   face    = 'bold',
                                   color   = 'black'),
        axis.text.x = element_text(family  = 'DejaVu Sans Mono', 
                                   face    = 'bold', 
                                   color   = 'black'),
        axis.title.x = element_text(family = 'DejaVu Sans Mono', 
                                    face   = 'bold', 
                                    color  = 'black'), 
        legend.position = 'none') 

ggsave(filename = '../Gráficos/EventosxDamnificados.png',
       plot = last_plot(),width = 12,height = 6)

# Graficos :: Frecuencia de eventos ---------------------------------------

vista = newdata %>%
  select(Evento) %>% 
  mutate(n = n()) %>% 
  group_by(Evento) %>% 
  summarise(total = sum(n)) %>% 
  arrange(desc(total))

newdata %>%
  select(Evento) %>% 
  mutate(n = n()) %>% 
  group_by(Evento) %>% 
  summarise(total = sum(n)) %>% 
  ggplot(aes(x = reorder(Evento,total) ,y = total, fill = total)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  labs(y = 'Frecuencia de eventos',
       x= '') +
  scale_y_discrete(limits = seq(0,9000000  ,length.out = 5),
                   labels = c("0","225k","450k","675k","900k")) + 
  scale_fill_viridis() +
  theme_light() + 
  theme(legend.text = element_text(family  = 'DejaVu Sans Mono',
                                   face    = 'bold', 
                                   color   = 'black'),
        axis.text.y = element_text(family  = 'DejaVu Sans Mono',
                                   face    = 'bold',
                                   color   = 'black'),
        axis.text.x = element_text(family  = 'DejaVu Sans Mono', 
                                   face    = 'bold', 
                                   color   = 'black'),
        axis.title.x = element_text(family = 'DejaVu Sans Mono', 
                                    face   = 'bold', 
                                    color  = 'black'), 
        legend.position = 'none') 

ggsave(filename = '../Gráficos/Frecuencia_Eventos.png',
       plot = last_plot(),width = 12,height = 6)


# Deslizamientos ----------------------------------------------------------

data %>%
  filter(Evento == 'LANDSLIDE') %>% 
  group_by(Departamento) %>% 
  summarise(total = sum(Afectados)) %>% 
  ggplot(aes(x = reorder(Departamento, total), y = total, fill = total)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  geom_text(aes(x = Departamento,y=total,label= total),
            vjust = 0.5,
            hjust = -0.05,
            size = 3.0) + 
  theme_minimal() + 
  scale_fill_viridis() + 
  theme(legend.text = element_text(family = 'DejaVu Sans Mono', face = 'bold', color = 'black'),
        axis.text.y = element_text(family = 'DejaVu Sans Mono', face = 'bold', color = 'black'),
        legend.position = 'none',
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) + 
  labs(x = '', y = '') + 
  ggtitle('')

ggsave(filename = '../Gráficos/Afectados_deslizamientos.png',
       plot = last_plot(),width = 12,height = 6)


# Inundaciones ------------------------------------------------------------
data %>%
  filter(Evento == 'FLOOD') %>% 
  group_by(Departamento) %>% 
  summarise(total = sum(Afectados)) %>% 
  ggplot(aes(x = reorder(Departamento, total), y = total, fill = total)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  geom_text(aes(x = Departamento,y=total,label= total),
            vjust = 0.5,
            hjust = -0.05,
            size = 3.0) + 
  theme_minimal() + 
  scale_fill_viridis() + 
  theme(legend.text = element_text(family = 'DejaVu Sans Mono', face = 'bold', color = 'black'),
        axis.text.y = element_text(family = 'DejaVu Sans Mono', face = 'bold', color = 'black'),
        legend.position = 'none',
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) + 
  labs(x = '', y = '') + 
  ggtitle('')


ggsave(filename = '../Gráficos/Afectados_inundaciones.png',
       plot = last_plot(),width = 12,height = 6)



# Mapa de inundaciones ----------------------------------------------------

data %>%
  filter(Evento == 'FLOOD') %>% 
  group_by(Departamento) %>% 
  summarise(total = sum(Afectados)) -> tabla

shp <- st_read('../SHP/departamentos_gtm/departamentos_gtm.shp') 
join <- left_join(x = shp,y = tabla, by = c('nombre'='Departamento'))  

join %>% 
  tm_shape() + 
  tm_layout(frame = F, legend.position = c(-0.27, 0)) +
  tm_fill(col = 'total',style = 'quantile',palette = 'viridis',title = 'Afectados por \n Inundaciones') + 
  tm_grid(n.x = 5, n.y = 5,lwd = 0.2) + 
  tm_scale_bar(position = c(0.6,0),width = 0.2,lwd = 0.2) + 
  tm_compass(position = c(0,0.8)) +
  tm_text(text = 'nombre',size = 0.6,fontface = 'bold')-> mapa


tmap_save(tm = mapa,
          filename = '../Mapas/Inundaciones_Deparmento.png',
          width = 11,height = 8)



# Mapa para lluvias -------------------------------------------------------
newdata %>% 
  filter(Evento == 'Lluvias') %>% 
  group_by(Departamento) %>%
  summarise(Total = sum(Damnificados)) -> tabla

shp <- st_read('../SHP/departamentos_gtm/departamentos_gtm.shp') 
join <- left_join(x = shp,y = tabla, by = c('nombre'='Departamento')) %>% 
  mutate(Total = if_else(is.na(Total), 0, Total))
anti_join(x = shp,y = tabla, by = c('nombre'='Departamento'))

join %>% 
  tm_shape() + 
  tm_layout(frame = F, legend.position = c(-0.27, 0)) +
  tm_fill(col = 'Total',style = 'quantile',palette = 'viridis',title = 'Afectados por \n LLuvias') + 
  tm_grid(n.x = 5, n.y = 5,lwd = 0.2) + 
  tm_scale_bar(position = c(0.6,0),width = 0.2,lwd = 0.2) + 
  tm_compass(position = c(0,0.8)) +
  tm_text(text = 'nombre',size = 0.6,fontface = 'bold')-> mapa


tmap_save(tm = mapa,
          filename = '../Mapas/Lluvias_Deparmento.png',
          width = 11,height = 8)




# Mapa para sequías -------------------------------------------------------

newdata %>% 
  filter(Evento == 'Sequías') %>% 
  group_by(Departamento) %>%
  summarise(Total = sum(Damnificados)) -> tabla

shp <- st_read('../SHP/departamentos_gtm/departamentos_gtm.shp') 
join <- left_join(x = shp,y = tabla, by = c('nombre'='Departamento')) %>% 
  mutate(Total = if_else(is.na(Total), 0, Total))
anti_join(x = shp,y = tabla, by = c('nombre'='Departamento'))

join %>% 
  tm_shape() + 
  tm_layout(frame = F, legend.position = c(-0.27, 0)) +
  tm_fill(col = 'Total',style = 'cat',palette = 'viridis',title = 'Afectados por \n Sequías') + 
  tm_grid(n.x = 5, n.y = 5,lwd = 0.2) + 
  tm_scale_bar(position = c(0.6,0),width = 0.2,lwd = 0.2) + 
  tm_compass(position = c(0,0.8)) +
  tm_text(text = 'nombre',size = 0.6,fontface = 'bold')-> mapa


tmap_save(tm = mapa,
          filename = '../Mapas/Sequías_Deparmento.png',
          width = 11,height = 8)


# Mapa para deslizamientos ------------------------------------------------

data %>%
  filter(Evento == 'LANDSLIDE') %>% 
  drop_na(Evento) %>% 
  group_by(Departamento) %>% 
  summarise(total = sum(Afectados)) -> tabla

shp <- st_read('../SHP/departamentos_gtm/departamentos_gtm.shp') 
join <- left_join(x = shp,y = tabla, by = c('nombre'='Departamento'))  

join %>% 
  tm_shape() + 
  tm_layout(frame = F, legend.position = c(-0.27, 0)) +
  tm_fill(col = 'total',style = 'quantile',palette = 'viridis',title = 'Afectados por \n Deslizamientos') + 
  tm_grid(n.x = 5, n.y = 5,lwd = 0.2) + 
  tm_scale_bar(position = c(0.6,0),width = 0.2,lwd = 0.2) + 
  tm_compass(position = c(0,0.8)) + 
  tm_text(text = 'nombre',size = 0.6,fontface = 'bold')-> mapa


tmap_save(tm = mapa,
          filename = '../Mapas/Deslizamientos_Deparmento.png',
          width = 11,height = 8)


# Mapa de Erupciones volcánicas -------------------------------------------
newdata %>% 
  filter(Evento == 'Erupciones volcánicas') %>% 
  group_by(Departamento) %>%
  summarise(Total = sum(Damnificados)) -> tabla

shp <- st_read('../SHP/departamentos_gtm/departamentos_gtm.shp') 
join <- left_join(x = shp,y = tabla, by = c('nombre'='Departamento')) %>% 
  mutate(Total = if_else(is.na(Total), 0, Total))
anti_join(x = shp,y = tabla, by = c('nombre'='Departamento'))

join %>% 
  tm_shape() + 
  tm_layout(frame = F, legend.position = c(-0.27, 0)) +
  tm_fill(col = 'Total',style = 'cat',palette = 'viridis',title = 'Afectados por \n Erupciones Volcánicas') + 
  tm_grid(n.x = 5, n.y = 5,lwd = 0.2) + 
  tm_scale_bar(position = c(0.6,0),width = 0.2,lwd = 0.2) + 
  tm_compass(position = c(0,0.8)) +
  tm_text(text = 'nombre',size = 0.6,fontface = 'bold')-> mapa


tmap_save(tm = mapa,
          filename = '../Mapas/Erupciones vilcánicas_Deparmento.png',
          width = 11,height = 8)
