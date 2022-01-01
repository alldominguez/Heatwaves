# Datos series de tiempo (2017-2019) - promedios diarios con datos horarios (material particulado)

#librerias 
pacman::p_load(tidyverse, lubridate, heatwaveR, readxl, rio, naniar, visdat, mice, patchwork,
               visdat)
getwd() #revisamos el espacio de trabajo

setwd("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data") # asignamos nuestro espacio de trabajo

#######################################
######### cargar los datos ############
#######################################


###################### PM10 ######################

# cerro navia
cn_pm10 <-read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Cerro Navia/cn_17_19_pm10.csv", sep =';', encoding ="UTF-8")

# santiago centro
sc_pm10 <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Santiago Centro/sc_17_19_pm10.csv", sep =';', encoding ="UTF-8")

# el bosque
eb_pm10 <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/El Bosque/eb_17_19_pm10.csv", sep =';', encoding ="UTF-8")

# revisamos la estructura de los datos
c(str(cn_pm10) , str(sc_pm10), str(eb_pm10)) # es necesario cambiar el formato 

# para facilitar el manejo de los datos cambiamos el nombre de las variables

cn_pm10_hourly <- cn_pm10 %>%  
  select(FECHA..YYMMDD., HORA..HHMM., Registros.validados, Registros.preliminares, 
         Registros.no.validados) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., pm10_valid = Registros.validados, 
         pm10_prelim = Registros.preliminares ,pm10_non_valid = Registros.no.validados)

sc_pm10_hourly <- sc_pm10 %>%  
  select(FECHA..YYMMDD., HORA..HHMM., Registros.validados, Registros.preliminares, 
         Registros.no.validados) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., pm10_valid = Registros.validados, 
         pm10_prelim = Registros.preliminares ,pm10_non_valid = Registros.no.validados)

eb_pm10_hourly <- eb_pm10 %>% 
  select(FECHA..YYMMDD., HORA..HHMM., Registros.validados, Registros.preliminares, 
         Registros.no.validados) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., pm10_valid = Registros.validados, 
         pm10_prelim = Registros.preliminares ,pm10_non_valid = Registros.no.validados)

c(str(cn_pm10_hourly) , str(sc_pm10_hourly), str(eb_pm10_hourly)) # es necesario cambiar el formato 

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

#cambiamos formato de temperatura
cn_pm10_hourly <- cn_pm10_hourly %>%  
  mutate(pm10_valid = as.numeric(gsub(",",".", cn_pm10_hourly$pm10_valid,fixed = TRUE)),
         pm10_prelim = as.numeric(gsub(",",".", cn_pm10_hourly$pm10_prelim,fixed = TRUE)),
         pm10_non_valid = as.numeric(gsub(",",".", cn_pm10_hourly$pm10_non_valid,fixed = TRUE))) # usamos expresiones regulares para cambiar , to . para que as.numeric funcione

sc_pm10_hourly <- sc_pm10_hourly %>%  
  mutate(pm10_valid = as.numeric(gsub(",",".", sc_pm10_hourly$pm10_valid,fixed = TRUE)),
         pm10_prelim = as.numeric(gsub(",",".", sc_pm10_hourly$pm10_prelim,fixed = TRUE)),
         pm10_non_valid = as.numeric(gsub(",",".", sc_pm10_hourly$pm10_non_valid,fixed = TRUE)))

eb_pm10_hourly  <- eb_pm10_hourly %>%  
  mutate(pm10_valid = as.numeric(gsub(",",".", eb_pm10_hourly$pm10_valid,fixed = TRUE)),
         pm10_prelim = as.numeric(gsub(",",".", eb_pm10_hourly$pm10_prelim,fixed = TRUE)),
         pm10_non_valid = as.numeric(gsub(",",".", eb_pm10_hourly$pm10_non_valid,fixed = TRUE)))

# revisamos la estructuras de datos
c(str(cn_pm10_hourly), str(sc_pm10_hourly), str(eb_pm10_hourly))

#######################################################
######### missing data - datos horarios ###############
#######################################################

library(visdat) # utilizamos la libreria visdat

cn_pm10_missing_hourly <- visdat::vis_miss(cn_pm10_hourly) + ggtitle("Cerro Navia") # cerro navia
sc_pm10_missing_hourly <- visdat::vis_miss(sc_pm10_hourly) + ggtitle("Santiago Centro")# santiago centro
eb_pm10_missing_hourly <- visdat::vis_miss(eb_pm10_hourly) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot_pm10_hourly <- cn_pm10_missing_hourly / sc_pm10_missing_hourly / eb_pm10_missing_hourly

# revisamos missing values en nuestra serie de tiempo horaria 
cn_pm10_hourly %>%
  is.na() %>%
  colSums()  # 59 NA

sc_pm10_hourly %>%  
  is.na() %>% 
  colSums()  # 23 NA

eb_pm10_hourly %>% 
  is.na() %>% 
  colSums() # 43 NA

# revisamos los datos por año 
cn_pm10_hourly %>%  select(year, pm10_valid) %>% group_by(year) %>% naniar::miss_var_summary() 
sc_pm10_hourly %>%  select(year, pm10_valid) %>% group_by(year) %>%naniar::miss_var_summary() 
eb_pm10_hourly %>%  select(year, pm10_valid) %>% group_by(year) %>% naniar::miss_var_summary() 

###############################################
######### calculo datos diarios ###############
###############################################

# calculamos temperatura promedio, maxima y minima para cada uno de los dias, utilzando los datos horarios
cn_pm10_daily <- cn_pm10_hourly %>% group_by(date) %>%  # calculo cerro navia
  summarise(pm10_mean = mean(pm10_valid , na.rm = FALSE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


sc_pm10_daily <- sc_pm10_hourly %>% group_by(date) %>%  # calculo santiago centro
  summarise(pm10_mean = mean(pm10_valid , na.rm = FALSE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


eb_pm10_daily <- eb_pm10_hourly %>% group_by(date) %>%  # calculo el bosque
  summarise(pm10_mean = mean(pm10_valid , na.rm = FALSE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


# revisamos la estructuras de datos
c(str(cn_pm10_daily), str(sc_pm10_daily), str(eb_pm10_daily))

# removemos los datos horarios
remove(cn_pm10_hourly, sc_pm10_hourly, eb_pm10_hourly)

#######################################################
######### missings data - datos diarios ###############
#######################################################

library(visdat) # utilizamos la libreria visdat

cn_missing_plot_daily <- visdat::vis_miss(cn_pm10_daily) + ggtitle("Cerro Navia") # cerro navia
sc_missing_plot_daily <- visdat::vis_miss(sc_pm10_daily) + ggtitle("Santiago Centro")# santiago centro
eb_missing_plot_daily <- visdat::vis_miss(eb_pm10_daily) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot_daily_pm10 <- cn_missing_plot_daily / sc_missing_plot_daily / eb_missing_plot_daily

# revisamos missing values en nuestra serie de tiempo horaria 
cn_pm10_daily %>%
  is.na() %>%
  colSums() # tenemos 59 NA

sc_pm10_daily %>%  
  is.na() %>% 
  colSums() # tenemos 23 NA

eb_pm10_daily %>% 
  is.na() %>% 
  colSums() # tenemos 43 NA 

# revisamos hidden missing values
cn_pm10_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

sc_pm10_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

eb_pm10_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

# reemplazamos por valores perdidos NA
cn_pm10_daily <- cn_pm10_daily %>% 
  replace_with_na(replace = list(pm10_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

sc_pm10_daily <- sc_pm10_daily %>% 
  replace_with_na(replace = list(pm10_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

eb_pm10_daily <- eb_pm10_daily %>% 
  replace_with_na(replace = list(pm10_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))


c(str(cn_pm10_daily), str(sc_pm10_daily), str(eb_pm10_daily))

############################################
######### exportamos los archivos ##########
############################################

rio::export(cn_pm10_daily, "cn_pm10_daily.xlsx") # exportamos cerro navia

rio::export(sc_pm10_daily, "sc_pm10_daily.xlsx") # exportamos santiago centro

rio::export(eb_pm10_daily, "eb_pm10_daily.xlsx") # exportamos el bosque

####################################
######### visualizaciones ##########
####################################

# scatterplot   

# generamos un grafico para revisar la serie de tiempo de las pm10 promedio
cn_pm10_scatter <- ggplot2::ggplot(cn_pm10_daily, mapping = aes(x = date, y = pm10_mean)) + 
  geom_point() + xlab("Date") + ylab("PM 10 ug/m3") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "red") + ggtitle("Cerro Navia") # si queremos añadir la tendencia

sc_pm10_scatter <- ggplot2::ggplot(sc_pm10_daily, mapping = aes(x = date, y = pm10_mean)) + 
  geom_point() + xlab("Date") + ylab("PM 10 ug/m3") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "red") + ggtitle("Santiago Centro")# si queremos añadir la tendencia

eb_pm10_scatter <- ggplot2::ggplot(eb_pm10_daily, mapping = aes(x = date, y = pm10_mean)) + 
  geom_point() + xlab("Date") + ylab("PM 10 ug/m3") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "red") + ggtitle("El Bosque")# si queremos añadir la tendencia

pm10_sinca_scatter <- cn_pm10_scatter / sc_pm10_scatter / eb_pm10_scatter

# boxplot 

cn_pm10_boxplot <- ggplot2::ggplot(cn_pm10_daily, mapping = aes(x = as.character(year), y = pm10_mean)) +
  geom_boxplot() + xlab("Year") + ylab("PM 10 ug/m3") + theme_classic() + ggtitle("Cerro Navia")

sc_pm10_boxplot <- ggplot2::ggplot(sc_pm10_daily, mapping = aes(x = as.character(year), y = pm10_mean)) +
  geom_boxplot() + xlab("Year") + ylab("PM 10 ug/m3") + theme_classic() + ggtitle("Santiago Centro")

eb_pm10_boxplot <- ggplot2::ggplot(eb_pm10_daily, mapping = aes(x = as.character(year), y = pm10_mean)) +
  geom_boxplot() + xlab("Year") + ylab("PM 10 ug/m3") + theme_classic() + ggtitle("El Bosque")

boxplot_p

