#librerias
pacman::p_load(httr, tidyverse, leaflet, janitor, readr, sparklyr, XML, readxl, xlsx)


#spark

library(sparklyr)
library(dplyr)
library(leaflet)
library(readxl)


#código
url<-	"https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/"

httr::GET(url)

ds <- jsonlite::fromJSON(url)
ds <- ds$ListaEESSPrecio


# EJERCICIO A

# I
ds <- ds %>% as_tibble() %>% clean_names() 

ds <- ds  %>% type_convert(locale = locale(decimal_mark = ",")) %>% view() %>% clean_names() 

view(ds)


# III

ds_2 <- ds %>%mutate(low_cost=rotulo%in%c("REPSOL","CAMPSA","BP", "SHELL","GALP", "CEPSA")) %>% view()

ds_2$low_cost[ds_2$low_cost == TRUE] <- 'No_Lowcost'
ds_2$low_cost[ds_2$low_cost == FALSE] <- 'Lowcost'

View(ds_2)

media_total_precios <- ds_2 %>% select(precio_bioetanol, precio_biodiesel, precio_gas_natural_comprimido,precio_gas_natural_licuado, precio_gases_licuados_del_petroleo, precio_gasoleo_a, precio_gasoleo_b, precio_gasoleo_premium, precio_gasolina_95_e10, precio_gasolina_95_e5, precio_gasolina_95_e5_premium, precio_gasolina_98_e10, precio_gasolina_98_e5, precio_hidrogeno, rotulo, idccaa, provincia) %>% 
  group_by(idccaa) %>% summarise(mean(precio_bioetanol, na.rm=TRUE), mean(precio_biodiesel, na.rm=TRUE), mean(precio_gas_natural_comprimido, na.rm=TRUE), mean(precio_gas_natural_licuado, na.rm=TRUE), mean(precio_gases_licuados_del_petroleo, na.rm=TRUE), mean(precio_gasoleo_a, na.rm=TRUE), mean(precio_gasoleo_b, na.rm=TRUE), mean(precio_gasoleo_premium, na.rm=TRUE), mean(precio_gasolina_95_e5, na.rm=TRUE), mean(precio_gasolina_95_e5_premium, na.rm=TRUE), mean(precio_gasolina_98_e10, na.rm=TRUE), mean(precio_gasolina_98_e5, na.rm=TRUE), mean(precio_hidrogeno, na.rm=TRUE)) %>% view()

# IV

ds_2 %>% select(rotulo, latitud, longitud_wgs84, precio_gasoleo_a, localidad, direccion) %>% 
  top_n(10, precio_gasoleo_a) %>%  leaflet() %>% addTiles() %>%  addCircleMarkers(lng = ~longitud_wgs84, lat = ~latitud, popup = ~rotulo, label = ~precio_gasoleo_a)

ds_2 %>% select(rotulo, latitud, longitud_wgs84, precio_gasoleo_a, localidad, direccion) %>% 
  top_n(-20, precio_gasoleo_a) %>%  leaflet() %>% addTiles() %>%  addCircleMarkers(lng = ~longitud_wgs84, lat = ~latitud, popup = ~rotulo, label = ~precio_gasoleo_a)

# V

write.csv(ds_2,"C:/Users/marki/OneDrive/Escritorio/Master business analytics/Posicionamiento empresarial en BIG DATA/Practica_final_Spark/low_cost_21846315.csv", row.names = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------------------------

#EJERCICIO B

# I

gasolineras_comunidad_madrid <- ds_2 %>% select(idccaa, provincia, low_cost) %>% 
  filter(idccaa == 13) %>% count(low_cost)  %>% view()

gasolineras_barcelona <- ds_2 %>% select(idccaa, provincia, low_cost) %>% 
  filter(idccaa == '09') %>% count(low_cost) %>% view()


# II

promedios_madrid <- ds_2 %>% select(idccaa, provincia, low_cost, precio_gasoleo_a, precio_gasolina_95_e5_premium) %>% 
  filter(idccaa == 13) %>% drop_na() %>% summarise(max(precio_gasoleo_a), min(precio_gasoleo_a), mean(precio_gasoleo_a), max(precio_gasolina_95_e5_premium), min(precio_gasolina_95_e5_premium), mean(precio_gasolina_95_e5_premium)) %>% view()

promedios_barcelos <- ds_2 %>% select(idccaa, provincia, low_cost, precio_gasoleo_a, precio_gasolina_95_e5_premium) %>% 
  filter(idccaa == '09') %>% drop_na() %>% summarise(max(precio_gasoleo_a), min(precio_gasoleo_a), mean(precio_gasoleo_a), max(precio_gasolina_95_e5_premium), min(precio_gasolina_95_e5_premium), mean(precio_gasolina_95_e5_premium)) %>% view()

# III

write.csv(promedios_madrid,"C:/Users/marki/OneDrive/Escritorio/Master business analytics/Posicionamiento empresarial en BIG DATA/Practica_final_Spark/informe_MAD_21846315.csv", row.names = FALSE)
write.csv(promedios_barcelos,"C:/Users/marki/OneDrive/Escritorio/Master business analytics/Posicionamiento empresarial en BIG DATA/Practica_final_Spark/informe_BCN_21846315.csv", row.names = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------------------------

# EJERCICIO C 

# I 

municipios <- ds_2 %>%  select(municipio, low_cost, precio_gasoleo_a, precio_gasolina_95_e5_premium) %>% group_by(municipio, low_cost) %>% 
  filter(!municipio %in% c('Madrid', 'Barcelona', 'Sevilla', 'Valencia')) %>% drop_na() %>% summarise(max(precio_gasoleo_a), min(precio_gasoleo_a), mean(precio_gasoleo_a), max(precio_gasolina_95_e5_premium), min(precio_gasolina_95_e5_premium), mean(precio_gasolina_95_e5_premium)) %>% view()

# II

write.csv(municipios,"C:/Users/marki/OneDrive/Escritorio/Master business analytics/Posicionamiento empresarial en BIG DATA/Practica_final_Spark/informe_no_grandes_ciudades_21846315.csv", row.names = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------------------------

# EJERCICIO D

# I

no_24H <- ds_2 %>% select(rotulo, horario) %>% filter(horario == 'L-D: 24H') %>% View()

# II

write.csv(no_24H,"C:/Users/marki/OneDrive/Escritorio/Master business analytics/Posicionamiento empresarial en BIG DATA/Practica_final_Spark/no_24_horas.csv", row.names = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------------------------

