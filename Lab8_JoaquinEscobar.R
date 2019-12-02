######## Laboratorio 8 ###########

library(tidyverse)
library(knitr)
library(kableExtra)
library(ggcorrplot)
library( rgl )
library(gganimate)

## ACTIVIDAD 1
> 
  Leemos las bases de datos ques se encuentran alojadas en https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/201
9-05-21

> 
  Primer base de datos *coast_vs_waste*: 

coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

coast_vs_waste %>% 
  sample_n(30, replace=T) %>% #Tomamos 20 observaciones de la malla
  kable() %>% # Damos formato a la tabla que imnprimiremos con la información
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed"),
                full_width = F,
                fixed_thead = T) %>%
  row_spec(0, color = "black") %>% 
  scroll_box(height = "250px") # Para poder desplazarnos a traves del scroll en la malla  

>
  Segunda base de datos *mismanaged_vs_gdp*: 

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

mismanaged_vs_gdp %>% 
  sample_n(30, replace=T) %>% #Tomamos 20 observaciones de la malla
  kable() %>% # Damos formato a la tabla que imnprimiremos con la información
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed"),
                full_width = F,
                fixed_thead = T) %>%
  row_spec(0, color = "black") %>% 
  scroll_box(height = "250px") # Para poder desplazarnos a traves del scroll en la malla  

>
Tercer base de datos *waste_vs_gdp*:
  
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

waste_vs_gdp %>% 
  sample_n(30, replace=T) %>% #Tomamos 20 observaciones de la malla
  kable() %>% # Damos formato a la tabla que imnprimiremos con la información
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed"),
                full_width = F,
                fixed_thead = T) %>%
  row_spec(0, color = "black") %>% 
  scroll_box(height = "250px") # Para poder desplazarnos a traves del scroll en la malla  

## ACTIVIDAD 2
> 
  Generar 4 gráficos de su elección a partir de las mallas de datos descargadas.
>
  Uno de los gráficos deberá ser animado, usando la librería gganimate.
>
  De preferencia generar gráficos no vistos en clase.

>
+ 
Grafico 1:

grafico1<-
  coast_vs_waste %>%
  filter(Entity %in% c("Brazil","Mexico","Argentina","Colombia","Chile"), Year=="2010") %>%
  select(-`Mismanaged plastic waste (tonnes)`) %>%
  mutate(`Coastal population`=-`Coastal population`) %>% 
  pivot_longer(-c(1:3), 
               names_to = "TipoPob",
               values_to = "CantidadPob") %>%
  #Hacemmos ajustes a la columna de TipoPob y agregamos niveles a la columna de Entity
  #para que los fatos en la grafica queden ordenados
  mutate(TipoPob = case_when(TipoPob =="Coastal population" ~ "Poblacion de Costa",
                             TipoPob =="Total population (Gapminder)" ~ "Poblacion Total"),
         Entity = factor(Entity, 
                         levels =c("Brazil","Mexico","Argentina","Colombia","Chile") )) 

#Realizamos la grafica 
ggplot(data=grafico1,
       aes(x = Entity, 
             y = CantidadPob, 
             fill = TipoPob, 
             group = TipoPob)) +
  geom_bar(data = subset(grafico1,TipoPob == "Poblacion de Costa"),
           stat = "identity") +
  geom_bar(data = subset(grafico1, TipoPob == "Poblacion Total"),
           stat = "identity") +
  coord_flip() +
  labs(x = "País", 
       y = "Cantidad de Población", 
       title = "Población Total y Costera",
       subtitle = "Para el año 2010") +
  scale_y_continuous(breaks = seq(-80e+06, 
                                  200e+06, 
                                  by = 40e+06),
                     labels = c(rev(seq(0, 
                                        80e+06, 
                                        by = 40e+06)), 
                                seq(40e+06, 
                                    200e+06, 
                                    by = 40e+06)))+
  theme_bw()+
  theme(legend.position = "bottom")

>
+
Grafico 2:
  
mismanaged_vs_gdp %>% 
  #Filtraremos la malla para tener solo dtos del año 2010 y aquellos paises con 
  #PIB per capita menor a 75000 USD y una RPMA menor a 0.2
  filter(Year == "2010",
         `GDP per capita, PPP (constant 2011 international $) (Rate)`<7.5e+04,
         `Per capita mismanaged plastic waste (kilograms per person per day)`< 0.2) %>% 
  #Dejamos unicamente las columnas que nos interesan
  select(Entity, 
         Year, 
         `Per capita mismanaged plastic waste (kilograms per person per day)`,
         `GDP per capita, PPP (constant 2011 international $) (Rate)`,
         `Total population (Gapminder)`) %>% 
  # Aplicamos el grafico en el eje de las x los residuos plasticos
  #En el eje de las y el PIB per capita
  ggplot(aes(`GDP per capita, PPP (constant 2011 international $) (Rate)`,
             `Per capita mismanaged plastic waste (kilograms per person per day)`))+
  #Coloreamos por pais y por tamaño de acuerdo a la cantidad de poblacion de cada pais
  geom_point(aes(col= Entity, 
                 size=`Total population (Gapminder)`)) +
  #Obtenemos la linea de tendencia
  geom_smooth(method="lm", 
              se=F) +
  #Propones los limetes de los ejes
  xlim(c(0,7.5e+04)) + 
  ylim(c(0,0.2)) +
  #Ingresamos las etiquetas de los ejes
  labs(x = "PIB per cápita (2011 USD constantes)", 
       y = "Residuos plásticos mal administrados per cápita (kg/día)", 
       title = "Relacion de residuos plasticos con el PIB per capita ",
       subtitle = "Para el año 2010",
       caption = "Fuente:  Our World in Data") +
  scale_x_continuous(labels=scales::dollar_format(prefix="$"))+
  theme_bw()+
  #Quitamos las leyendas
  theme(legend.position = "none")

Del grafico 2 podemos apreciar que la relacion entre residuos plasticos mal administrados percapita y el PIB percapita es inversa, es decir,
entre mayor es el PIB per capita menor es el valor de los residuos plasticos.

>
+
Grafico 3:

waste_vs_gdp %>%
  #Filtramos la malla para tener datos del 2010
    filter(Year=="2010") %>%
  #Unicamente seleccionamos las variables que nos interesan
    select("Residuos Plasticos per capita" = `Per capita plastic waste (kilograms per person per day)`,
           "PIB per capita"=`GDP per capita, PPP (constant 2011 international $) (constant 2011 international $)`,
           "Poblacion Total"=`Total population (Gapminder)`) %>%
     #Obtenemos la correlaciones entre variables de la malla resultante
     cor(use = "complete.obs") %>% 
  #Obtenemos el correlograma entre las variables
     ggcorrplot(hc.order = TRUE, 
                type = "lower",
                outline.col = "white",
                ggtheme = theme_dark(),
                colors = c("blue", "white", "red")) +
     labs(title = "Residuos VS PIB per capita",
          subtitle = "Correlograma con información del 2010",
          caption = "Fuente:  Our World in Data")+
     theme(legend.position="right")

Del grafico 3 podemos observar que existe una relacion inversa entre la poblacion total con la cantidad de residuos plasticos per capita y con el PIB per capita. Hay una relación directa, la cual es debil entre la cantidad de residuos plasticos per capita con el PIB per capita.

>
+
Grafico 4:

  #Unimos la malla mismanaged_vs_gdp y waste_vs_gdp
  inner_join(mismanaged_vs_gdp,
             waste_vs_gdp,
             by=c("Entity","Code","Year","Total population (Gapminder)")) %>% 
  #Seleccionamos el año 2010 y los paises de Mexico y España
  filter(Year == "2010", Entity %in% c("Mexico","Spain")) %>%
  #Seleccionamos las columnas que nos interesan
  select(Entity,
         "Residuo Plastico per capita (kg/dia)"=`Per capita plastic waste (kilograms per person per day)`,
         "Residuo Platico con mal manejo per capita (Kg/dia)"=`Per capita mismanaged plastic waste (kilograms per person per day)`) %>%
  #Pasamos de wide a long
  pivot_longer(-Entity,
               names_to = "Tipo",
               values_to = "Cantidad") %>% 
  #Aplicamos el grafico
  ggplot(aes(x = Entity,
             y = Cantidad,
             fill = Entity))+
  geom_bar(stat='identity') +
  labs(title = "Medición: {closest_state}", 
       x = "País", 
       y = "Residuos Plasticos per capita (kg/día)",
       caption = "Fuente:  Our World in Data") + 
  transition_states(Tipo) +
  theme_grey() +
  theme(legend.position = "none",
        title = element_text(size = 16,
                             colour = "red"),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"))
#Guardamos la grafica animada
anim_save("GraficaElegida.gif")

El grafico 4 presenta una comparación entre México y España en cuanto a sus razones de residuos plasticos per capita.
  
## **ACTIVIDAD 3**
>
Elegir el gráfico que mejor le parezca y subirlo a twitter con la etiqueta (hashtag) #datosdemiercoles .
>
+ Subir el grfico con un breve comentario y la liga de Github de su código.
+ Incluir la captura de pantalla como comprobante.

include_graphics("./CapturaTwitter.png")

## **ACTIVIDAD 4**
>Incluir los siguientes datos de la cuenta creada en github:
  >
  + Nombre de usuario
+ La liga del repositorio
+  Una captura de pantalla del repositorio
