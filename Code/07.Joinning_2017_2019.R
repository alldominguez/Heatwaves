# Combinar todos los datos por comuna 

#librerias 
pacman::p_load(tidyverse, lubridate, heatwaveR, readxl, rio, naniar, visdat, mice, patchwork)

getwd() #revisamos el espacio de trabajo

setwd("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data") # asignamos nuestro espacio de trabajo
 
  
#######################################
########### Cerro Navia ###############
#######################################
  
# cargamos los datos 2017-2019 para datos ambientales (SINCA) y datos de mortalidad (DEIS)

# mortalidad para todas las causas 2017-2019 
cn_mortality_all_causes <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/DEIS_2017_2019/mortality/mortality_cn/mortality_cn_all_causes_2017_2019.xlsx")

#mortalidad causas cardiovascular 2017-2019
cn_mortality_cardio <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/DEIS_2017_2019/mortality/mortality_cn/mortality_cn_cardio_2017_2019.xlsx")

#mortalidad causas respiratorias 2017-2019
cn_mortality_resp <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/DEIS_2017_2019/mortality/mortality_cn/mortality_cn_resp_2017_2019.xlsx")

# datos ambientales 

# temperatura 
cn_temperature <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/cn_temp_daily.xlsx")

# pm10
cn_pm10 <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/cn_pm10_daily.xlsx")

# pm25 
cn_pm25 <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/cn_pm25_daily.xlsx")

# ozono
cn_o3 <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/cn_o3_daily.xlsx")

# velocidad del viento 
cn_ws <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/cn_ws_daily.xlsx")

# humedad relativa
cn_rh <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/cn_rh_daily.xlsx")

# revisamos el formato

c(str(cn_temperature), str(cn_pm10), str(cn_pm25), str(cn_o3), str(cn_ws), str(cn_rh))

#######################################
######### cambio de formatos ##########
#######################################

# DEIS
cn_mortality_all_causes$date <- lubridate::ymd(cn_mortality_all_causes$date)
cn_mortality_cardio$date <- lubridate::ymd(cn_mortality_cardio$date)
cn_mortality_resp$date <- lubridate::ymd(cn_mortality_resp$date)

# SINCA
cn_temperature$date <- lubridate::ymd(cn_temperature$date) 
cn_pm10$date <- lubridate::ymd(cn_pm10$date)
cn_pm25$date <- lubridate::ymd(cn_pm25$date)
cn_o3$date <- lubridate::ymd(cn_o3$date)
cn_ws$date <- lubridate::ymd(cn_ws$date)
cn_rh$date <- lubridate::ymd(cn_rh$date)

#######################################
############## Join ################### #### variables ambientales
#######################################
library(tidyverse)

# primero creamos una lista donde incluimos a todos nuestros dataset
cn_list <- list(cn_temperature, cn_pm10, cn_pm25, cn_o3, cn_ws, cn_rh)

# combinamos todos los dataset, utilizando las variables data, year, month y day, ya que son las que se repiten en nuestro dataframe
cn_sinca <- cn_list %>% reduce(full_join, by= c('date', 'year', 'month', 'day'))

#reordenamos las variables usando select
cn_sinca <- cn_sinca %>%
  select(date, year, month, day, temp_mean:rh_min)


#######################################
############## Join ################### #### variables ambientales + mortalidad
#######################################

# mortalidad por todas las causas 
cn_all_causes_sinca_2017_2019 <- full_join(cn_mortality_all_causes, cn_sinca , by = 'date')

# mortalidad por causas cardiovasculares
cn_cardio_sinca_2017_2019 <- full_join(cn_mortality_cardio, cn_sinca , by = 'date')

# mortalidad por causas respiratorias
cn_resp_2017_2019 <- full_join(cn_mortality_resp, cn_sinca , by = 'date')

# revisamos la estructura de la nueva base de datos
c(str(cn_all_causes_sinca_2017_2019), str(cn_cardio_sinca_2017_2019), str(cn_resp_2017_2019))


#############################################
############## exportamos ################### #### cerro navia ####
############################################

rio::export(cn_all_causes_sinca_2017_2019, "cn_all_causes_sinca_2017_2019.xlsx") # exportamos mortalidad por todas las causas

rio::export(cn_cardio_sinca_2017_2019, "cn_cardio_sinca_2017_2019.xlsx") # exportamos mortalidad por causas cardiovasculares

rio::export(cn_resp_2017_2019 , "cn_resp_2017_2019.xlsx") # exportamos mortalidad por causas cardiovasculares
 

###############################################################################################################################################################################

#######################################
########### El Bosque #################
#######################################

# cargamos los datos 2017-2019 para datos ambientales (SINCA) y datos de mortalidad (DEIS)

# mortalidad para todas las causas 2017-2019 
eb_mortality_all_causes <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/DEIS_2017_2019/mortality/mortality_eb/mortality_eb_all_causes_2017_2019.xlsx")

#mortalidad causas cardiovascular 2017-2019
eb_mortality_cardio <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/DEIS_2017_2019/mortality/mortality_eb/mortality_eb_cardio_2017_2019.xlsx")

#mortalidad causas respiratorias 2017-2019
eb_mortality_resp <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/DEIS_2017_2019/mortality/mortality_eb/mortality_eb_resp_2017_2019.xlsx")


# datos ambientales 

# temperatura 
eb_temperature <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/eb_temp_daily.xlsx")

# pm10
eb_pm10 <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/eb_pm10_daily.xlsx")

# pm25 
eb_pm25 <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/eb_pm25_daily.xlsx")

# ozono
eb_o3 <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/eb_o3_daily.xlsx")

# velocidad del viento 
eb_ws <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/eb_ws_daily.xlsx")

# humedad relativa
eb_rh <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/eb_rh_daily.xlsx")

# revisamos el formato

c(str(eb_temperature), str(eb_pm10), str(eb_pm25), str(eb_o3), str(eb_ws), str(eb_rh))


#######################################
######### cambio de formatos ########## 
#######################################

# DEIS
eb_mortality_all_causes$date <- lubridate::ymd(eb_mortality_all_causes$date)
eb_mortality_cardio$date <- lubridate::ymd(eb_mortality_cardio$date)
eb_mortality_resp$date <- lubridate::ymd(eb_mortality_resp$date)

# SINCA
eb_temperature$date <- lubridate::ymd(eb_temperature$date) 
eb_pm10$date <- lubridate::ymd(eb_pm10$date)
eb_pm25$date <- lubridate::ymd(eb_pm25$date)
eb_o3$date <- lubridate::ymd(eb_o3$date)
eb_ws$date <- lubridate::ymd(eb_ws$date)
eb_rh$date <- lubridate::ymd(eb_rh$date)


#######################################
############## Join ################### #### variables ambientales
#######################################
library(tidyverse)

# primero creamos una lista donde incluimos a todos nuestros dataset
eb_list <- list(eb_temperature, eb_pm10, eb_pm25, eb_o3, eb_ws, eb_rh)

# combinamos todos los dataset, utilizando las variables data, year, month y day, ya que son las que se repiten en nuestro dataframe
eb_sinca <- eb_list %>% reduce(full_join, by= c('date', 'year', 'month', 'day'))

#reordenamos las variables usando select
eb_sinca <- eb_sinca %>%
  select(date, year, month, day, temp_mean:rh_min)

#######################################
############## Join ################### #### variables ambientales + mortalidad
#######################################

# mortalidad por todas las causas 
eb_all_causes_sinca_2017_2019 <- full_join(eb_mortality_all_causes, eb_sinca , by = 'date')

# mortalidad por causas cardiovasculares
eb_cardio_sinca_2017_2019 <- full_join(eb_mortality_cardio, eb_sinca , by = 'date')

# mortalidad por causas respiratorias
eb_resp_2017_2019 <- full_join(eb_mortality_resp, eb_sinca , by = 'date')

# revisamos la estructura de la nueva base de datos
c(str(eb_all_causes_sinca_2017_2019), str(eb_cardio_sinca_2017_2019), str(eb_resp_2017_2019))


#############################################
############## exportamos ################### #### cerro navia ####
############################################

rio::export(eb_all_causes_sinca_2017_2019, "eb_all_causes_sinca_2017_2019.xlsx") # exportamos mortalidad por todas las causas

rio::export(eb_cardio_sinca_2017_2019, "eb_cardio_sinca_2017_2019.xlsx") # exportamos mortalidad por causas cardiovasculares

rio::export(eb_resp_2017_2019 , "eb_resp_2017_2019.xlsx") # exportamos mortalidad por causas cardiovasculares


###########################################
########### Santiago Centro ###############
##########################################

# cargamos los datos 2017-2019 para datos ambientales (SINCA) y datos de mortalidad (DEIS)

# mortalidad para todas las causas 2017-2019 
sc_mortality_all_causes <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/DEIS_2017_2019/mortality/mortality_sc/mortality_sc_all_causes_2017_2019.xlsx")

#mortalidad causas cardiovascular 2017-2019
sc_mortality_cardio <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/DEIS_2017_2019/mortality/mortality_sc/mortality_sc_cardio_2017_2019.xlsx")

#mortalidad causas respiratorias 2017-2019
sc_mortality_resp <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/DEIS_2017_2019/mortality/mortality_sc/mortality_sc_resp_2017_2019.xlsx")

# datos ambientales 

# temperatura 
sc_temperature <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/sc_temp_daily.xlsx")

# pm10
sc_pm10 <- read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/sc_pm10_daily.xlsx")

# pm25 
sc_pm25 <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/sc_pm25_daily.xlsx")

# ozono
sc_o3 <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/sc_o3_daily.xlsx")

# velocidad del viento 
sc_ws <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/sc_ws_daily.xlsx")

# humedad relativa
sc_rh <-read_xlsx("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/SINCA_2017_2019/sc_rh_daily.xlsx")

# revisamos el formato

c(str(sc_temperature), str(sc_pm10), str(sc_pm25), str(sc_o3), str(sc_ws), str(sc_rh))


#######################################
######### cambio de formatos ########## 
#######################################

# DEIS
sc_mortality_all_causes$date <- lubridate::ymd(sc_mortality_all_causes$date)
sc_mortality_cardio$date <- lubridate::ymd(sc_mortality_cardio$date)
sc_mortality_resp$date <- lubridate::ymd(sc_mortality_resp$date)

# SINCA
sc_temperature$date <- lubridate::ymd(sc_temperature$date) 
sc_pm10$date <- lubridate::ymd(sc_pm10$date)
sc_pm25$date <- lubridate::ymd(sc_pm25$date)
sc_o3$date <- lubridate::ymd(sc_o3$date)
sc_ws$date <- lubridate::ymd(sc_ws$date)
sc_rh$date <- lubridate::ymd(sc_rh$date)

#######################################
############## Join ################### #### variables ambientales
#######################################
library(tidyverse)

# primero creamos una lista donde incluimos a todos nuestros dataset
sc_list <- list(sc_temperature, sc_pm10, sc_pm25, sc_o3, sc_ws, sc_rh)

# combinamos todos los dataset, utilizando las variables data, year, month y day, ya que son las que se repiten en nuestro dataframe
sc_sinca <- sc_list %>% reduce(full_join, by= c('date', 'year', 'month', 'day'))

#reordenamos las variables usando select
sc_sinca <- sc_sinca %>%
  select(date, year, month, day, temp_mean:rh_min)

#######################################
############## Join ################### #### variables ambientales + mortalidad
#######################################

# mortalidad por todas las causas 
sc_all_causes_sinca_2017_2019 <- full_join(sc_mortality_all_causes, sc_sinca , by = 'date')

# mortalidad por causas cardiovasculares
sc_cardio_sinca_2017_2019 <- full_join(sc_mortality_cardio, sc_sinca , by = 'date')

# mortalidad por causas respiratorias
sc_resp_2017_2019 <- full_join(sc_mortality_resp, sc_sinca , by = 'date')

# revisamos la estructura de la nueva base de datos
c(str(sc_all_causes_sinca_2017_2019), str(sc_cardio_sinca_2017_2019), str(sc_resp_2017_2019))

#############################################
############## exportamos ################### #### cerro navia ####
############################################

rio::export(sc_all_causes_sinca_2017_2019, "sc_all_causes_sinca_2017_2019.xlsx") # exportamos mortalidad por todas las causas

rio::export(sc_cardio_sinca_2017_2019, "sc_cardio_sinca_2017_2019.xlsx") # exportamos mortalidad por causas cardiovasculares

rio::export(sc_resp_2017_2019 , "sc_resp_2017_2019.xlsx") # exportamos mortalidad por causas cardiovasculares



















####################################
######### visualizaciones ##########
####################################

# scatterplot   

# generamos un grafico para revisar la serie de tiempo de las temperaturas promedio
cn_temperature_scatter <- ggplot2::ggplot(cn_temperature, mapping = aes(x = date, y = temp_mean)) + 
  geom_point() + xlab("Date") + ylab("Temperature \n (ºC)") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "red") # si queremos añadir la tendencia

# generamos un grafico para revisar la serie de tiempo de las pm10 promedio
cn_pm10_scatter <- ggplot2::ggplot(cn_pm10, mapping = aes(x = date, y = pm10_mean)) + 
  geom_point() + xlab("Date") + ylab("PM10 \n (ug/m3)") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "purple")  # si queremos añadir la tendencia

# generamos un grafico para revisar la serie de tiempo de las pm25promedio
cn_pm25_scatter <- ggplot2::ggplot(cn_pm25, mapping = aes(x = date, y = pm25_mean)) + 
  geom_point() + xlab("Date") + ylab("PM25 \n (ug/m3)") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "purple")  # si queremos añadir la tendencia

# generamos un grafico para revisar la serie de tiempo de las velocidad del viento
cn_ws_scatter <- ggplot2::ggplot(cn_ws, mapping = aes(x = date, y = ws_mean)) + 
  geom_point() + xlab("Date") + ylab("Wind Speed \n (m/s)") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "blue")  # si queremos añadir la tendencia

# generamos un grafico para revisar la serie de tiempo de las humedad relativa
cn_rh_scatter <- ggplot2::ggplot(cn_rh, mapping = aes(x = date, y = rh_mean)) + 
  geom_point() + xlab("Date") + ylab("Relative Humidity \n (%) ") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "blue")  # si queremos añadir la tendencia 

# generamos un grafico para revisar la serie de tiempo de las ozono
cn_o3_scatter <- ggplot2::ggplot(cn_o3, mapping = aes(x = date, y = o3_mean)) + 
  geom_point() + xlab("Date") + ylab("Ozone \n (ppb)") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "purple")  # si queremos añadir la tendencia

cn_serie <- cn_temperature_scatter / cn_pm10_scatter / cn_pm25_scatter / cn_o3_scatter / cn_ws_scatter / cn_rh_scatter 


use_git_config(user.name = "alldominguez", user.email = "aidominguezmontoya@gmail.com")






