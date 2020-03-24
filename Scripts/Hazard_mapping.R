# Insumos -----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(sf)
library(tmap)
library(extrafont)
library(viridis)
library(forcats)

# Lectura de datos --------------------------------------------------------

readxl::read_xlsx('../Datos/Datos-Historicos.xlsx',sheet = 1)[2:25] %>% 
  mutate(Afectados    = as.integer(Afectados),
         Evento       = as.factor(Evento),
         Departamento = as.factor(Departamento),
         Municipio    = as.factor(Municipio)) %>% 
  drop_na(Afectados) -> data 
  

data %>%
  group_by(Departamento, Evento) %>% 
  summarise(total = sum(Afectados)) %>% 
  ggplot(aes(x = reorder(Departamento, total), y = total, fill = Evento), position = 'fill') + 
  geom_bar(stat = 'identity') 




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


# Elaboración de mapas ----------------------------------------------------
# Inundaciones
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


# Deslizamientos
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





