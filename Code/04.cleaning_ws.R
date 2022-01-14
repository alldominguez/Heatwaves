# Datos series de tiempo (2017-2019) - promedios diarios con datos horarios (velocidad del viento)

#librerias 
pacman::p_load(tidyverse, lubridate, heatwaveR, readxl, rio, naniar, visdat, mice, patchwork)

getwd() #revisamos el espacio de trabajo

setwd("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data") # asignamos nuestro espacio de trabajo

#######################################
######### cargar los datos ############
#######################################

##################################################### Temperatura #####################################################

# cerro navia
cn_ws <-read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Cerro Navia/cn_17_19_ws.csv", sep =';', encoding ="UTF-8")

# santiago centro
sc_ws <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Santiago Centro/sc_17_19_ws.csv", sep =';', encoding ="UTF-8")

# el bosque
eb_ws <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/El Bosque/eb_17_19_ws.csv", sep =';', encoding ="UTF-8")

#revisar la estructura de los datos
c(str(cn_ws), str(sc_ws), str(eb_ws)) # 

# para facilitar el manejo de los datos cambiamos el nombre de las variables
cn_ws_hourly <- cn_ws %>% select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., ws = X) 


sc_ws_hourly <- sc_ws %>% select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., ws = X) 


eb_ws_hourly <- eb_ws %>% select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., ws = X) 

# removemos los dabase previos
remove(cn_ws, sc_ws, eb_ws) 







