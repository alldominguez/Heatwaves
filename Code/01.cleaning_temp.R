# Datos series de tiempo (2017-2019) - promedios diarios con datos horarios (temperaturas)

 
### keychain : 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

#librerias 
pacman::p_load(tidyverse, lubridate, heatwaveR, readxl, rio, naniar, visdat, mice, patchwork)

getwd() #revisamos el espacio de trabajo

setwd("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data") # asignamos nuestro espacio de trabajo

#######################################
######### cargar los datos ############
#######################################

##################################################### Temperatura #####################################################

# cerro navia
cn_temperature <-read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Cerro Navia/cn_17_19_temp.csv", sep =';', encoding ="UTF-8")

# santiago centro
sc_temperature <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/Santiago Centro/sc_17_19_temp.csv", sep =';', encoding ="UTF-8")

# el bosque
eb_temperature <- read.csv("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data/El Bosque/eb_17_19_temp.csv", sep =';', encoding ="UTF-8")


#revisar la estructura de los datos
c(str(cn_temperature), str(sc_temperature), str(eb_temperature)) # 

# para facilitar el manejo de los datos cambiamos el nombre de las variables
cn_temperature_hourly <- cn_temperature %>% select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., temp = X) 


sc_temperature_hourly <- sc_temperature %>% select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., temp = X) 
  
  
eb_temperature_hourly <- eb_temperature %>% select(FECHA..YYMMDD., HORA..HHMM., X) %>% 
  rename(date = FECHA..YYMMDD., hour = HORA..HHMM., temp = X) 
  
  
# removemos los dabase previos
remove(cn_temperature, sc_temperature, eb_temperature) 

#######################################
######### cambio de formatos ##########
#######################################

# cambiamos formato de date
cn_temperature_hourly$date <- lubridate::ymd(cn_temperature_hourly$date) # la forma más facil

sc_temperature_hourly$date <- lubridate::ymd(sc_temperature_hourly$date)

eb_temperature_hourly$date <- lubridate::ymd(eb_temperature_hourly$date)


# generamos 3 nuevas variables , year, month, day 
cn_temperature_hourly <- cn_temperature_hourly %>% mutate(year = lubridate::year(date),
                                                          month = lubridate::month(date),
                                                          day = lubridate::day(date))

sc_temperature_hourly <- sc_temperature_hourly %>% mutate(year = lubridate::year(date),
                                                          month = lubridate::month(date),
                                                          day = lubridate::day(date))

eb_temperature_hourly <- eb_temperature_hourly %>% mutate(year = lubridate::year(date),
                                                          month = lubridate::month(date),
                                                          day = lubridate::day(date))

c(str(cn_temperature_hourly), str(sc_temperature_hourly), str(eb_temperature_hourly))

#cambiamos formato de temperatura
cn_temperature_hourly <- cn_temperature_hourly %>%  
  mutate(temp = as.numeric(gsub(",",".", cn_temperature_hourly$temp,fixed=TRUE))) # usamos expresiones regulares para cambiar , to . para que as.numeric funcione

sc_temperature_hourly <- sc_temperature_hourly %>%  
  mutate(temp = as.numeric(gsub(",",".", sc_temperature_hourly$temp,fixed=TRUE)))

eb_temperature_hourly <- eb_temperature_hourly %>%  
  mutate(temp = as.numeric(gsub(",",".", eb_temperature_hourly$temp,fixed=TRUE)))


#######################################
######### missing data  ###############
#######################################

library(visdat) # utilizamos la libreria visdat

cn_missing_plot <- visdat::vis_miss(cn_temperature_hourly) + ggtitle("Cerro Navia") # cerro navia
sc_missing_plot <- visdat::vis_miss(sc_temperature_hourly) + ggtitle("Santiago Centro")# santiago centro
eb_missing_plot <- visdat::vis_miss(eb_temperature_hourly) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot_hourly <- cn_missing_plot / sc_missing_plot / eb_missing_plot

# revisamos missing values en nuestra serie de tiempo horaria 
cn_temperature_hourly %>%
  is.na() %>%
  colSums() # tenemos 110 NA

sc_temperature_hourly %>%  
  is.na() %>% 
  colSums() # tenemos 478 NA

eb_temperature_hourly %>% 
  is.na() %>% 
  colSums() # tenemos 317 NA 

# revisamos los datos por año 
cn_temperature_hourly %>%  select(year, temp) %>% group_by(year) %>% naniar::miss_var_summary() 
sc_temperature_hourly %>%  select(year, temp) %>% group_by(year) %>%naniar::miss_var_summary() 
eb_temperature_hourly %>%  select(year, temp) %>% group_by(year) %>% naniar::miss_var_summary() 

# revisamos si existen hidden missing values
cn_temperature_hourly %>% naniar::miss_scan_count(search = list("NA", "N/A", "na", ""))
sc_temperature_hourly %>% naniar::miss_scan_count(search = list("NA", "N/A", "na", ""))
eb_temperature_hourly %>% naniar::miss_scan_count(search = list("NA", "N/A", "na", ""))

###############################################
######### calculo datos diarios ###############
###############################################

# calculamos temperatura promedio, maxima y minima para cada uno de los dias, utilzando los datos horarios

cn_temperature_daily <- cn_temperature_hourly %>% group_by(date) %>%  # calculo cerro navia
  summarise(temp_mean = mean(temp , na.rm = FALSE),
            temp_max = max(temp, na.rm = TRUE),
            temp_min = min(temp, na.rm = TRUE)) %>% 
  mutate(year = lubridate::year(date),
                           month = lubridate::month(date),
                           day = lubridate::day(date))


sc_temperature_daily <- sc_temperature_hourly %>% group_by(date) %>%  # calculo santiago centro
  summarise(temp_mean = mean(temp , na.rm = TRUE),
            temp_max = max(temp, na.rm = TRUE),
            temp_min = min(temp, na.rm = TRUE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))
 

eb_temperature_daily <- eb_temperature_hourly %>% group_by(date) %>% # calculo el bosque
  summarise(temp_mean = mean(temp , na.rm = TRUE),
            temp_max = max(temp , na.rm = TRUE),
            temp_min = min(temp , na.rm = TRUE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


###################################################
######### revisamos missings values ###############
###################################################

library(visdat) # utilizamos la libreria visdat

cn_missing_plot <- visdat::vis_miss(cn_temperature_daily) + ggtitle("Cerro Navia") # cerro navia
sc_missing_plot <- visdat::vis_miss(sc_temperature_daily) + ggtitle("Santiago Centro")# santiago centro
eb_missing_plot <- visdat::vis_miss(eb_temperature_daily) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot <- cn_missing_plot / sc_missing_plot / eb_missing_plot

# revisamos missing values en nuestra serie de tiempo horaria 
cn_temperature_daily %>%
  is.na() %>%
  colSums() # tenemos 15 NA

sc_temperature_daily %>%  
  is.na() %>% 
  colSums() # tenemos 15 NA

eb_temperature_daily %>% 
  is.na() %>% 
  colSums() # tenemos 2 NA 


# revisamos hidden missing values
cn_temperature_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

sc_temperature_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

eb_temperature_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

# reemplazamos por valores perdidos NA

cn_temperature_daily <- cn_temperature_daily %>% 
  replace_with_na(replace = list(temp_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 temp_max = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 temp_min = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))


sc_temperature_daily <- sc_temperature_daily %>% 
  replace_with_na(replace = list(temp_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 temp_max = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 temp_min = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

eb_temperature_daily <- eb_temperature_daily %>% 
  replace_with_na(replace = list(temp_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 temp_max = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 temp_min = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))
 

############################################
######### exportamos los archivos ##########
############################################

rio::export(cn_temperature_daily, "cn_temp_daily.xlsx") # exportamos cerro navia

rio::export(sc_temperature_daily, "sc_temp_daily.xlsx") # exportamos santiago centro

rio::export(eb_temperature_daily, "eb_temp_daily.xlsx") # exportamos el bosque

####################################
######### visualizaciones ##########
####################################
  
# scatter plot

# generamos un grafico para revisar la serie de tiempo de las temperaturas promedio
cn_temperature_scatter <- ggplot2::ggplot(cn_temperature_daily, mapping = aes(x = date, y = temp_mean)) + 
  geom_point() + xlab("Date") + ylab("Temperature ºC") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
               geom_smooth() + ggtitle("Cerro Navia") # si queremos añadir la tendencia

sc_temperature_scatter <- ggplot2::ggplot(sc_temperature_daily, mapping = aes(x = date, y = temp_mean)) + 
  geom_point() + xlab("Date") + ylab("Temperature ºC") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth() + ggtitle("Santiago Centro") # si queremos añadir la tendencia

eb_temperature_scatter <- ggplot2::ggplot(eb_temperature_daily, mapping = aes(x = date, y = temp_mean)) + 
  geom_point() + xlab("Date") + ylab("Temperature ºC") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth() + ggtitle("El Bosque")  # si queremos añadir la tendencia

cn_temperature_scatter / sc_temperature_scatter / eb_temperature_scatter


