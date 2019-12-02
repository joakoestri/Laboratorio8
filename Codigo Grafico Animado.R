#Leemos las mallas
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

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