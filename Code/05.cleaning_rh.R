# Datos series de tiempo (2017-2019) - promedios diarios con datos horarios (humedad relativa)

#librerias 
pacman::p_load(tidyverse, lubridate, heatwaveR, readxl, rio, naniar, visdat, mice, patchwork)

getwd() #revisamos el espacio de trabajo

setwd("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data") # asignamos nuestro espacio de trabajo

#######################################
######### cargar los datos ############
#######################################

##################################################### Temperatura #####################################################


# cerro navia
cn_rh <-read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Cerro Navia/cn_17_19_hr.csv", sep =';', encoding ="UTF-8")

# santiago centro
sc_rh <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Santiago Centro/sc_17_19_rh.csv", sep =';', encoding ="UTF-8")

# el bosque
eb_rh <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/El Bosque/eb_17_19_rh.csv", sep =';', encoding ="UTF-8")

#revisar la estructura de los datos
c(str(cn_rh), str(sc_rh), str(eb_rh)) # 

# para facilitar el manejo de los datos cambiamos el nombre de las variables
cn_rh_hourly <- cn_rh %>% select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., rh = X) 


sc_rh_hourly <- sc_rh %>% select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., rh = X) 


eb_rh_hourly <- eb_rh %>% select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., rh = X) 


# removemos los dabase previos
remove(cn_rh, sc_rh, eb_rh) 

#######################################
######### cambio de formatos ##########
#######################################

# cambiamos formato de date
cn_rh_hourly$date <- lubridate::ymd(cn_rh_hourly$date) # la forma más facil

sc_rh_hourly$date <- lubridate::ymd(sc_rh_hourly$date)

eb_rh_hourly$date <- lubridate::ymd(eb_rh_hourly$date)


# generamos 3 nuevas variables , year, month, day 
cn_rh_hourly <- cn_rh_hourly %>% mutate(year = lubridate::year(date),
                                                          month = lubridate::month(date),
                                                          day = lubridate::day(date))

sc_rh_hourly <- sc_rh_hourly %>% mutate(year = lubridate::year(date),
                                                          month = lubridate::month(date),
                                                          day = lubridate::day(date))

eb_rh_hourly <- eb_rh_hourly %>% mutate(year = lubridate::year(date),
                                                          month = lubridate::month(date),
                                                          day = lubridate::day(date))

c(str(cn_rh_hourly), str(sc_rh_hourly), str(eb_rh_hourly))

#cambiamos formato de temperatura
cn_rh_hourly <- cn_rh_hourly %>%  
  mutate(rh = as.numeric(gsub(",",".", cn_rh_hourly$rh,fixed=TRUE))) # usamos expresiones regulares para cambiar , to . para que as.numeric funcione

sc_rh_hourly <- sc_rh_hourly %>%  
  mutate(rh = as.numeric(gsub(",",".", sc_rh_hourly$rh,fixed=TRUE)))

eb_rh_hourly <- eb_rh_hourly %>%  
  mutate(rh = as.numeric(gsub(",",".", eb_rh_hourly$rh,fixed=TRUE)))

#######################################
######### missing data  ###############
#######################################

library(visdat) # utilizamos la libreria visdat

cn_missing_plot <- visdat::vis_miss(cn_rh_hourly) + ggtitle("Cerro Navia") # cerro navia
sc_missing_plot <- visdat::vis_miss(sc_rh_hourly) + ggtitle("Santiago Centro")# santiago centro
eb_missing_plot <- visdat::vis_miss(eb_rh_hourly) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot_hourly <- cn_missing_plot / sc_missing_plot / eb_missing_plot

# revisamos missing values en nuestra serie de tiempo horaria 
cn_rh_hourly %>%
  is.na() %>%
  colSums() # tenemos 104 NA

sc_rh_hourly %>%  
  is.na() %>% 
  colSums() # tenemos 736 NA

eb_rh_hourly %>% 
  is.na() %>% 
  colSums() # tenemos 322 NA 

# revisamos los datos por año 
cn_rh_hourly %>%  select(year, rh) %>% group_by(year) %>% naniar::miss_var_summary() 
sc_rh_hourly %>%  select(year, rh) %>% group_by(year) %>%naniar::miss_var_summary() 
eb_rh_hourly %>%  select(year, rh) %>% group_by(year) %>% naniar::miss_var_summary() 

# revisamos si existen hidden missing values
cn_rh_hourly %>% naniar::miss_scan_count(search = list("NA", "N/A", "na", ""))
sc_rh_hourly %>% naniar::miss_scan_count(search = list("NA", "N/A", "na", ""))
eb_rh_hourly %>% naniar::miss_scan_count(search = list("NA", "N/A", "na", ""))

###############################################
######### calculo datos diarios ###############
###############################################

# calculamos humedad relativa promedio, maxima y minima para cada uno de los dias, utilzando los datos horarios

cn_rh_daily <- cn_rh_hourly %>% group_by(date) %>%  # calculo cerro navia
  summarise(rh_mean = mean(rh , na.rm = FALSE),
            rh_max = max(rh, na.rm = TRUE),
            rh_min = min(rh, na.rm = TRUE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


sc_rh_daily <- sc_rh_hourly %>% group_by(date) %>%  # calculo santiago centro
  summarise(rh_mean = mean(rh , na.rm = TRUE),
            rh_max = max(rh, na.rm = TRUE),
            rh_min = min(rh, na.rm = TRUE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


eb_rh_daily <- eb_rh_hourly %>% group_by(date) %>% # calculo el bosque
  summarise(rh_mean = mean(rh , na.rm = TRUE),
            rh_max = max(rh , na.rm = TRUE),
            rh_min = min(rh , na.rm = TRUE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))

###################################################
######### revisamos missings values ###############
###################################################
 
library(visdat) # utilizamos la libreria visdat

cn_missing_plot <- visdat::vis_miss(cn_rh_daily) + ggtitle("Cerro Navia") # cerro navia
sc_missing_plot <- visdat::vis_miss(sc_rh_daily) + ggtitle("Santiago Centro")# santiago centro
eb_missing_plot <- visdat::vis_miss(eb_rh_daily) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot <- cn_missing_plot / sc_missing_plot / eb_missing_plot

# revisamos missing values en nuestra serie de tiempo horaria 
cn_rh_daily %>%
  is.na() %>%
  colSums() # tenemos 14 NA

sc_rh_daily %>%  
  is.na() %>% 
  colSums() # tenemos 13 NA

eb_rh_daily %>% 
  is.na() %>% 
  colSums() # tenemos 2 NA 


# revisamos hidden missing values
cn_rh_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

sc_rh_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

eb_rh_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

# reemplazamos por valores perdidos NA

cn_rh_daily <- cn_rh_daily %>% 
  replace_with_na(replace = list(rh_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 rh_max = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 rh_min = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))


sc_rh_daily <- sc_rh_daily %>% 
  replace_with_na(replace = list(rh_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 rh_max = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 rh_min = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

eb_rh_daily <- eb_rh_daily %>% 
  replace_with_na(replace = list(rh_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 rh_max = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 rh_min = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

############################################
######### exportamos los archivos ##########
############################################

rio::export(cn_rh_daily, "cn_rh_daily.xlsx") # exportamos cerro navia

rio::export(sc_rh_daily, "sc_rh_daily.xlsx") # exportamos santiago centro

rio::export(eb_rh_daily, "eb_rh_daily.xlsx") # exportamos el bosque

####################################
######### visualizaciones ##########
#################################### 

# scatter plot

# generamos un grafico para revisar la serie de tiempo de las humedad relativa
cn_rh_scatter <- ggplot2::ggplot(cn_rh_daily, mapping = aes(x = date, y = rh_mean)) + 
  geom_point() + xlab("Date") + ylab("Relative Humidity") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth() + ggtitle ("Cerro Navia")# si queremos añadir la tendencia 

sc_rh_scatter <- ggplot2::ggplot(sc_rh_daily, mapping = aes(x = date, y = rh_mean)) + 
  geom_point() + xlab("Date") + ylab("Relative Humidity") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth() + ggtitle("Santiago Centro") # si queremos añadir la tendencia

eb_rh_scatter <- ggplot2::ggplot(eb_rh_daily, mapping = aes(x = date, y = rh_mean)) + 
  geom_point() + xlab("Date") + ylab("Relative Humidiry") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth() + ggtitle("El Bosque") # si queremos añadir la tendencia

cn_rh_scatter / sc_rh_scatter / eb_rh_scatter




