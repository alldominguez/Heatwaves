# Datos series de tiempo (2017-2019) - promedios diarios con datos horarios (ozono)

#librerias 
pacman::p_load(tidyverse, lubridate, heatwaveR, readxl, rio, naniar, visdat, mice, patchwork,
               visdat)
getwd() #revisamos el espacio de trabajo

setwd("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data") # asignamos nuestro espacio de trabajo

#######################################
######### cargar los datos ############
#######################################


###################### O3 ######################

# cerro navia
cn_o3 <-read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Cerro Navia/cn_17_19_o3.csv", sep =';', encoding ="UTF-8")

# santiago centro
sc_o3 <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Santiago Centro/sc_17_19_o3.csv", sep =';', encoding ="UTF-8")

# el bosque
eb_o3 <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/El Bosque/eb_17_19_o3.csv", sep =';', encoding ="UTF-8")

# revisamos la estructura de los datos
c(str(cn_o3) , str(sc_o3), str(eb_o3)) # es necesario cambiar el formato 

# para facilitar el manejo de los datos cambiamos el nombre de las variables

cn_o3_hourly <- cn_o3 %>%  
  select(FECHA..YYMMDD., HORA..HHMM., Registros.validados, Registros.preliminares, 
         Registros.no.validados) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., o3_valid = Registros.validados, 
         o3_prelim = Registros.preliminares , o3_non_valid = Registros.no.validados)

sc_o3_hourly <- sc_o3 %>%  
  select(FECHA..YYMMDD., HORA..HHMM., Registros.validados, Registros.preliminares, 
         Registros.no.validados) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., o3_valid = Registros.validados, 
         o3_prelim = Registros.preliminares , o3_non_valid = Registros.no.validados)

eb_o3_hourly <- eb_o3 %>% 
  select(FECHA..YYMMDD., HORA..HHMM., Registros.validados, Registros.preliminares, 
         Registros.no.validados) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., o3_valid = Registros.validados, 
         o3_prelim = Registros.preliminares , o3_non_valid = Registros.no.validados)

c(str(cn_o3_hourly) , str(sc_o3_hourly), str(eb_o3_hourly)) # es necesario cambiar el formato 

# removemos los dataset iniciales
remove(cn_o3, sc_o3, eb_o3)

#######################################
######### cambio de formatos ##########
#######################################

# cambiamos formato de date
cn_o3_hourly$date <- lubridate::ymd(cn_o3_hourly$date) # la forma más facil

sc_o3_hourly$date <- lubridate::ymd(sc_o3_hourly$date)

eb_o3_hourly$date <- lubridate::ymd(eb_o3_hourly$date)

# generamos 3 nuevas variables , year, month, day 
cn_o3_hourly <- cn_o3_hourly %>% mutate(year = lubridate::year(date),
                                            month = lubridate::month(date),
                                            day = lubridate::day(date))

sc_o3_hourly <- sc_o3_hourly %>% mutate(year = lubridate::year(date),
                                            month = lubridate::month(date),
                                            day = lubridate::day(date))

eb_o3_hourly <- eb_o3_hourly %>% mutate(year = lubridate::year(date),
                                            month = lubridate::month(date),
                                            day = lubridate::day(date))

#cambiamos formato de temperatura
cn_o3_hourly <- cn_o3_hourly %>%  
  mutate(o3_valid = as.numeric(gsub(",",".", cn_o3_hourly$o3_valid,fixed = TRUE)),
         o3_prelim = as.numeric(gsub(",",".", cn_o3_hourly$o3_prelim,fixed = TRUE)),
         o3_non_valid = as.numeric(gsub(",",".", cn_o3_hourly$o3_non_valid,fixed = TRUE))) # usamos expresiones regulares para cambiar , to . para que as.numeric funcione

sc_o3_hourly <- sc_o3_hourly %>%  
  mutate(o3_valid = as.numeric(gsub(",",".", sc_o3_hourly$o3_valid,fixed = TRUE)),
         o3_prelim = as.numeric(gsub(",",".", sc_o3_hourly$o3_prelim,fixed = TRUE)),
         o3_non_valid = as.numeric(gsub(",",".", sc_o3_hourly$o3_non_valid,fixed = TRUE)))

eb_o3_hourly  <- eb_o3_hourly %>%  
  mutate(o3_valid = as.numeric(gsub(",",".", eb_o3_hourly$o3_valid,fixed = TRUE)),
         o3_prelim = as.numeric(gsub(",",".", eb_o3_hourly$o3_prelim,fixed = TRUE)),
         o3_non_valid = as.numeric(gsub(",",".", eb_o3_hourly$o3_non_valid,fixed = TRUE)))

# revisamos la estructuras de datos
c(str(cn_o3_hourly), str(sc_o3_hourly), str(eb_o3_hourly))

#######################################################
######### missing data - datos horarios ###############
#######################################################

library(visdat) # utilizamos la libreria visdat

cn_o3_missing_hourly <- visdat::vis_miss(cn_o3_hourly) + ggtitle("Cerro Navia") # cerro navia
sc_o3_missing_hourly <- visdat::vis_miss(sc_o3_hourly) + ggtitle("Santiago Centro")# santiago centro
eb_o3_missing_hourly <- visdat::vis_miss(eb_o3_hourly) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot_o3_hourly <- cn_o3_missing_hourly / sc_o3_missing_hourly / eb_o3_missing_hourly

# revisamos missing values en nuestra serie de tiempo horaria 
cn_o3_hourly %>%
  is.na() %>%
  colSums()  

sc_o3_hourly %>%  
  is.na() %>% 
  colSums()  

eb_o3_hourly %>% 
  is.na() %>% 
  colSums() 

# revisamos los datos por año 
cn_o3_hourly %>%  select(year, o3_valid) %>% group_by(year) %>% naniar::miss_var_summary() 
sc_o3_hourly %>%  select(year, o3_valid) %>% group_by(year) %>%naniar::miss_var_summary() 
eb_o3_hourly %>%  select(year, o3_valid) %>% group_by(year) %>% naniar::miss_var_summary() 


###############################################
######### calculo datos diarios ###############
###############################################

# calculamos temperatura promedio, maxima y minima para cada uno de los dias, utilzando los datos horarios
cn_o3_daily <- cn_o3_hourly %>% group_by(date) %>%  # calculo cerro navia
  summarise(o3_mean = mean(o3_valid , na.rm = FALSE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


sc_o3_daily <- sc_o3_hourly %>% group_by(date) %>%  # calculo santiago centro
  summarise(o3_mean = mean(o3_valid , na.rm = FALSE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


eb_o3_daily <- eb_o3_hourly %>% group_by(date) %>%  # calculo el bosque
  summarise(o3_mean = mean(o3_valid , na.rm = FALSE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


# revisamos la estructuras de datos
c(str(cn_o3_daily), str(sc_o3_daily), str(eb_o3_daily))

# removemos los datos horarios
remove(cn_o3_hourly, sc_o3_hourly, eb_o3_hourly)

#######################################################
######### missings data - datos diarios ###############
#######################################################

library(visdat) # utilizamos la libreria visdat

cn_missing_plot_daily <- visdat::vis_miss(cn_o3_daily) + ggtitle("Cerro Navia") # cerro navia
sc_missing_plot_daily <- visdat::vis_miss(sc_o3_daily) + ggtitle("Santiago Centro")# santiago centro
eb_missing_plot_daily <- visdat::vis_miss(eb_o3_daily) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot_daily_o3 <- cn_missing_plot_daily / sc_missing_plot_daily / eb_missing_plot_daily

# revisamos missing values en nuestra serie de tiempo horaria 
cn_o3_daily %>%
  is.na() %>%
  colSums() # tenemos 73 NA

sc_o3_daily %>%  
  is.na() %>% 
  colSums() # tenemos 277 NA

eb_o3_daily %>% 
  is.na() %>% 
  colSums() # tenemos 216 NA 

# revisamos hidden missing values
cn_o3_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

sc_o3_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

eb_o3_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

# reemplazamos por valores perdidos NA
cn_o3_daily <- cn_o3_daily %>% 
  replace_with_na(replace = list(o3_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

sc_o3_daily <- sc_o3_daily %>% 
  replace_with_na(replace = list(o3_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

eb_o3_daily <- eb_o3_daily %>% 
  replace_with_na(replace = list(o3_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))


c(str(cn_o3_daily), str(sc_o3_daily), str(eb_o3_daily))

############################################
######### exportamos los archivos ##########
############################################

rio::export(cn_o3_daily, "cn_o3_daily.xlsx") # exportamos cerro navia

rio::export(sc_o3_daily, "sc_o3_daily.xlsx") # exportamos santiago centro

rio::export(eb_o3_daily, "eb_o3_daily.xlsx") # exportamos el bosque


####################################
######### visualizaciones ##########
####################################

# scatterplot   

# generamos un grafico para revisar la serie de tiempo de las pm10 promedio
cn_o3_scatter <- ggplot2::ggplot(cn_o3_daily, mapping = aes(x = date, y = o3_mean)) + 
  geom_point() + xlab("Date") + ylab("Ozone ug/m3") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "red") + ggtitle("Cerro Navia") # si queremos añadir la tendencia

sc_o3_scatter <- ggplot2::ggplot(sc_o3_daily, mapping = aes(x = date, y = o3_mean)) + 
  geom_point() + xlab("Date") + ylab("Ozone ug/m3") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "red") + ggtitle("Santiago Centro")# si queremos añadir la tendencia

eb_o3_scatter <- ggplot2::ggplot(eb_o3_daily, mapping = aes(x = date, y = o3_mean)) + 
  geom_point() + xlab("Date") + ylab("Ozone ug/m3") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth(colour = "red") + ggtitle("El Bosque")# si queremos añadir la tendencia

# figura 1
o3_sinca_scatter <- cn_o3_scatter / sc_o3_scatter / eb_o3_scatter

# boxplot 
cn_o3_boxplot <- ggplot2::ggplot(cn_o3_daily, mapping = aes(x = as.character(year), y = o3_mean)) +
  geom_boxplot() + scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 25)) + 
  xlab("Year") + ylab("Ozone ug/m3") + theme_classic() + ggtitle("Cerro Navia") 

sc_o3_boxplot <- ggplot2::ggplot(sc_o3_daily, mapping = aes(x = as.character(year), y = o3_mean)) +
  geom_boxplot() + scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 25)) + 
  xlab("Year") + ylab("Ozone ug/m3") + theme_classic() + ggtitle("Santiago Centro") 

eb_o3_boxplot <- ggplot2::ggplot(eb_o3_daily, mapping = aes(x = as.character(year), y = o3_mean)) +
  geom_boxplot() + scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 25)) + 
  xlab("Year") + ylab("Ozone ug/m3") + theme_classic() + ggtitle("El Bosque") 

# figura 2
o3_sinca_boxplot <- cn_o3_boxplot / sc_o3_boxplot / eb_o3_boxplot







