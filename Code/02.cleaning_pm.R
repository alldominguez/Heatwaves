# Datos series de tiempo (2017-2019) - promedios diarios con datos horarios (material particulado)

#librerias 
pacman::p_load(tidyverse, lubridate, heatwaveR, readxl, rio, naniar, visdat, mice, patchwork,
               visdat)


#######################################
######### cargar los datos ############
#######################################


###################### PM10 ######################

# cerro navia
cn_pm10 <-read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Cerro Navia/cn_17_19_temp.csv", sep =';', encoding ="UTF-8")

# santiago centro
sc_pm10 <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Santiago Centro/sc_17-19_temp.csv", sep =';', encoding ="UTF-8")

# el bosque
eb_pm10 <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/El Bosque/eb_17_19_temp.csv", sep =';', encoding ="UTF-8")

# revisamos la estructura de los datos
c(str(cn_pm10) , str(sc_pm10), str(eb_pm10)) # es necesario cambiar el formato 

# para facilitar el manejo de los datos cambiamos el nombre de las variables

cn_pm10_hourly <- cn_pm10 %>%  # este simbolo nos sirve para reempazar <- y asignar de inmediato nuestras acciones al objeto a la izquierda del simbolo
  select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., pm10 = X)

sc_pm10_hourly <- sc_pm10 %>%  
  select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., pm10 = X)

eb_pm10_hourly <- eb_pm10 %>% 
  select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., pm10 = X)
  
# removemos los dataset iniciales
remove(cn_pm10, sc_pm10, eb_pm10)

#######################################
######### cambio de formatos ##########
#######################################

# cambiamos formato de date
cn_pm10_hourly$date <- lubridate::ymd(cn_pm10_hourly$date) # la forma más facil

sc_pm10_hourly$date <- lubridate::ymd(sc_pm10_hourly$date)

eb_pm10_hourly$date <- lubridate::ymd(eb_pm10_hourly$date)

# generamos 3 nuevas variables , year, month, day 
cn_pm10_hourly <- cn_pm10_hourly %>% mutate(year = lubridate::year(date),
                                                             month = lubridate::month(date),
                                                             day = lubridate::day(date))

sc_pm10_hourly <- sc_pm10_hourly %>% mutate(year = lubridate::year(date),
                                                              month = lubridate::month(date),
                                                              day = lubridate::day(date))

eb_pm10_hourly <- eb_pm10_hourly %>% mutate(year = lubridate::year(date),
                                                              month = lubridate::month(date),
                                                              day = lubridate::day(date))

str(sc_pm10_hourly) # revisamos la estructuras de datos


#cambiamos formato de temperatura
cn_pm10_hourly <- cn_pm10_hourly %>%  
  mutate(pm10 = as.numeric(gsub(",",".", cn_pm10_hourly$pm10,fixed=TRUE))) # usamos expresiones regulares para cambiar , to . para que as.numeric funcione

sc_pm10_hourly <- sc_pm10_hourly %>%  
  mutate(pm10 = as.numeric(gsub(",",".", sc_pm10_hourly$pm10,fixed=TRUE)))

eb_pm10_hourly  <- eb_pm10_hourly %>%  
  mutate(pm10 = as.numeric(gsub(",",".", eb_pm10_hourly$pm10,fixed=TRUE)))


#######################################
######### missing data  ###############
#######################################

library(visdat) # utilizamos la libreria visdat

cn_pm10_missing <- visdat::vis_miss(cn_pm10_hourly) + ggtitle("Cerro Navia") # cerro navia
sc_pm10_missing <- visdat::vis_miss(sc_pm10_hourly) + ggtitle("Santiago Centro")# santiago centro
eb_pm10_missing <- visdat::vis_miss(eb_pm10_hourly) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot <- cn_pm10_missing / sc_pm10_missing / eb_pm10_missing

# revisamos missing values en nuestra serie de tiempo horaria 
cn_pm10_hourly %>%
  is.na() %>%
  colSums()  # 110 NA

sc_pm10_hourly %>%  
  is.na() %>% 
  colSums()  # 478 NA

eb_pm10_hourly %>% 
  is.na() %>% 
  colSums() # 317 NA

# revisamos los datos por año 
cn_temperature_hourly %>%  select(year, temp) %>% group_by(year) %>% naniar::miss_var_summary() 
sc_temperature_hourly %>%  select(year, temp) %>% group_by(year) %>%naniar::miss_var_summary() 
eb_temperature_hourly %>%  select(year, temp) %>% group_by(year) %>% naniar::miss_var_summary() 

# revisamos si existen hidden missing values

cn_temperature_hourly %>% naniar::miss_scan_count(search = list("NA", "N/A", "na", ""))



















###################### PM25 ######################



