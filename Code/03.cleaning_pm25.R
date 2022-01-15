# Datos series de tiempo (2017-2019) - promedios diarios con datos horarios (material particulado)

#librerias 
pacman::p_load(tidyverse, lubridate, heatwaveR, readxl, rio, naniar, visdat, mice, patchwork,
               visdat)
getwd() #revisamos el espacio de trabajo

setwd("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data") # asignamos nuestro espacio de trabajo

#######################################
######### cargar los datos ############
#######################################
   

###################### PM25 ######################

# cerro navia
cn_pm25 <-read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Cerro Navia/cn_17_19_pm25.csv", sep =';', encoding ="UTF-8")

# santiago centro
sc_pm25 <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Santiago Centro/sc_17_19_pm25.csv", sep =';', encoding ="UTF-8")

# el bosque
eb_pm25 <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/El Bosque/eb_17_19_pm25.csv", sep =';', encoding ="UTF-8")

# revisamos la estructura de los datos
c(str(cn_pm25) , str(sc_pm25), str(eb_pm25)) # es necesario cambiar el formato 

# para facilitar el manejo de los datos cambiamos el nombre de las variables

cn_pm25_hourly <- cn_pm25 %>%  
  select(FECHA..YYMMDD., HORA..HHMM., Registros.validados, Registros.preliminares, 
         Registros.no.validados) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., pm25_valid = Registros.validados, 
         pm25_prelim = Registros.preliminares ,pm25_non_valid = Registros.no.validados)

sc_pm25_hourly <- sc_pm25 %>%  
  select(FECHA..YYMMDD., HORA..HHMM., Registros.validados, Registros.preliminares, 
         Registros.no.validados) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., pm25_valid = Registros.validados, 
         pm25_prelim = Registros.preliminares ,pm25_non_valid = Registros.no.validados)

eb_pm25_hourly <- eb_pm25 %>% 
  select(FECHA..YYMMDD., HORA..HHMM., Registros.validados, Registros.preliminares, 
         Registros.no.validados) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., pm25_valid = Registros.validados, 
         pm25_prelim = Registros.preliminares ,pm25_non_valid = Registros.no.validados)

c(str(cn_pm25_hourly) , str(sc_pm25_hourly), str(eb_pm25_hourly)) # es necesario cambiar el formato 

# removemos los dataset iniciales
remove(cn_pm25, sc_pm25, eb_pm25)

#######################################
######### cambio de formatos ##########
#######################################

# cambiamos formato de date
cn_pm25_hourly$date <- lubridate::ymd(cn_pm25_hourly$date) # la forma más facil

sc_pm25_hourly$date <- lubridate::ymd(sc_pm25_hourly$date)

eb_pm25_hourly$date <- lubridate::ymd(eb_pm25_hourly$date)

# generamos 3 nuevas variables , year, month, day 
cn_pm25_hourly <- cn_pm25_hourly %>% mutate(year = lubridate::year(date),
                                            month = lubridate::month(date),
                                            day = lubridate::day(date))

sc_pm25_hourly <- sc_pm25_hourly %>% mutate(year = lubridate::year(date),
                                            month = lubridate::month(date),
                                            day = lubridate::day(date))

eb_pm25_hourly <- eb_pm25_hourly %>% mutate(year = lubridate::year(date),
                                            month = lubridate::month(date),
                                            day = lubridate::day(date))


#cambiamos formato de temperatura
cn_pm25_hourly <- cn_pm25_hourly %>%  
  mutate(pm25_valid = as.numeric(gsub(",",".", cn_pm25_hourly$pm25_valid,fixed = TRUE)),
         pm25_prelim = as.numeric(gsub(",",".", cn_pm25_hourly$pm25_prelim,fixed = TRUE)),
         pm25_non_valid = as.numeric(gsub(",",".", cn_pm25_hourly$pm25_non_valid,fixed = TRUE))) # usamos expresiones regulares para cambiar , to . para que as.numeric funcione

sc_pm25_hourly <- sc_pm25_hourly %>%  
  mutate(pm25_valid = as.numeric(gsub(",",".", sc_pm25_hourly$pm25_valid,fixed = TRUE)),
         pm25_prelim = as.numeric(gsub(",",".", sc_pm25_hourly$pm25_prelim,fixed = TRUE)),
         pm25_non_valid = as.numeric(gsub(",",".", sc_pm25_hourly$pm25_non_valid,fixed = TRUE)))

eb_pm25_hourly  <- eb_pm25_hourly %>%  
  mutate(pm25_valid = as.numeric(gsub(",",".", eb_pm25_hourly$pm25_valid,fixed = TRUE)),
         pm25_prelim = as.numeric(gsub(",",".", eb_pm25_hourly$pm25_prelim,fixed = TRUE)),
         pm25_non_valid = as.numeric(gsub(",",".", eb_pm25_hourly$pm25_non_valid,fixed = TRUE)))

# revisamos la estructuras de datos
c(str(cn_pm25_hourly), str(sc_pm25_hourly), str(eb_pm25_hourly))

#######################################################
######### missing data - datos horarios ###############
#######################################################

library(visdat) # utilizamos la libreria visdat

cn_pm25_missing_hourly <- visdat::vis_miss(cn_pm25_hourly) + ggtitle("Cerro Navia") # cerro navia
sc_pm25_missing_hourly <- visdat::vis_miss(sc_pm25_hourly) + ggtitle("Santiago Centro")# santiago centro
eb_pm25_missing_hourly <- visdat::vis_miss(eb_pm25_hourly) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot_pm25_hourly <- cn_pm25_missing_hourly / sc_pm25_missing_hourly / eb_pm25_missing_hourly

# revisamos missing values en nuestra serie de tiempo horaria 
cn_pm25_hourly %>%
  is.na() %>%
  colSums()  # 59 NA

sc_pm25_hourly %>%  
  is.na() %>% 
  colSums()  # 23 NA

eb_pm25_hourly %>% 
  is.na() %>% 
  colSums() # 43 NA

# revisamos los datos por año 
cn_pm25_hourly %>%  select(year, pm25_valid) %>% group_by(year) %>% naniar::miss_var_summary() 
sc_pm25_hourly %>%  select(year, pm25_valid) %>% group_by(year) %>%naniar::miss_var_summary() 
eb_pm25_hourly %>%  select(year, pm25_valid) %>% group_by(year) %>% naniar::miss_var_summary() 

###############################################
######### calculo datos diarios ###############
###############################################

# calculamos temperatura promedio, maxima y minima para cada uno de los dias, utilzando los datos horarios
cn_pm25_daily <- cn_pm25_hourly %>% group_by(date) %>%  # calculo cerro navia
  summarise(pm25_mean = mean(pm25_valid , na.rm = FALSE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


sc_pm25_daily <- sc_pm25_hourly %>% group_by(date) %>%  # calculo santiago centro
  summarise(pm25_mean = mean(pm25_valid , na.rm = FALSE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


eb_pm25_daily <- eb_pm25_hourly %>% group_by(date) %>%  # calculo el bosque
  summarise(pm25_mean = mean(pm25_valid , na.rm = FALSE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


# revisamos la estructuras de datos
c(str(cn_pm25_daily), str(sc_pm25_daily), str(eb_pm25_daily))

# removemos los datos horarios
remove(cn_pm25_hourly, sc_pm25_hourly, eb_pm25_hourly)

#######################################################
######### missings data - datos diarios ###############
#######################################################

library(visdat) # utilizamos la libreria visdat

cn_missing_plot_daily <- visdat::vis_miss(cn_pm25_daily) + ggtitle("Cerro Navia") # cerro navia
sc_missing_plot_daily <- visdat::vis_miss(sc_pm25_daily) + ggtitle("Santiago Centro")# santiago centro
eb_missing_plot_daily <- visdat::vis_miss(eb_pm25_daily) + ggtitle("El Bosque") # el bosque

# revisamos missing values en nuestra serie de tiempo horaria 
cn_pm25_daily %>%
  is.na() %>%
  colSums() # tenemos 84 NA

sc_pm25_daily %>%  
  is.na() %>% 
  colSums() # tenemos 79 NA

eb_pm25_daily %>% 
  is.na() %>% 
  colSums() # tenemos 141 NA 

# revisamos hidden missing values
cn_pm25_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

sc_pm25_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

eb_pm25_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

# reemplazamos por valores perdidos NA
cn_pm25_daily <- cn_pm25_daily %>% 
  replace_with_na(replace = list(pm25_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

sc_pm10_daily <- sc_pm25_daily %>% 
  replace_with_na(replace = list(pm25_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

eb_pm25_daily <- eb_pm25_daily %>% 
  replace_with_na(replace = list(pm25_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

c(str(cn_pm25_daily), str(sc_pm25_daily), str(eb_pm25_daily))

############################################
######### exportamos los archivos ##########
############################################

rio::export(cn_pm25_daily, "cn_pm25_daily.xlsx") # exportamos cerro navia

rio::export(sc_pm25_daily, "sc_pm25_daily.xlsx") # exportamos santiago centro

rio::export(eb_pm25_daily, "eb_pm25_daily.xlsx") # exportamos el bosque

####################################
######### visualizaciones ##########
####################################
 
# scatterplot   

# generamos un grafico para revisar la serie de tiempo de las pm25 promedio
cn_pm25_scatter <- ggplot2::ggplot(cn_pm25_daily, mapping = aes(x = date, y = pm25_mean)) + 
  geom_point() + xlab("Date") + ylab("PM 25 ug/m3") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "red") + ggtitle("Cerro Navia") # si queremos añadir la tendencia

sc_pm25_scatter <- ggplot2::ggplot(sc_pm25_daily, mapping = aes(x = date, y = pm25_mean)) + 
  geom_point() + xlab("Date") + ylab("PM 25 ug/m3") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "red") + ggtitle("Santiago Centro")# si queremos añadir la tendencia

eb_pm25_scatter <- ggplot2::ggplot(eb_pm25_daily, mapping = aes(x = date, y = pm25_mean)) + 
  geom_point() + xlab("Date") + ylab("PM 25 ug/m3") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "red") + ggtitle("El Bosque")# si queremos añadir la tendencia


