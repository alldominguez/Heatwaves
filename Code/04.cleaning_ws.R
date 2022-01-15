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

#######################################
######### cambio de formatos ##########
#######################################
  
# cambiamos formato de date
cn_ws_hourly$date <- lubridate::ymd(cn_ws_hourly$date) # la forma más facil

sc_ws_hourly$date <- lubridate::ymd(sc_ws_hourly$date)

eb_ws_hourly$date <- lubridate::ymd(eb_ws_hourly$date)

# generamos 3 nuevas variables , year, month, day 
cn_ws_hourly <- cn_ws_hourly %>% mutate(year = lubridate::year(date),
                                                          month = lubridate::month(date),
                                                          day = lubridate::day(date))

sc_ws_hourly <- sc_ws_hourly %>% mutate(year = lubridate::year(date),
                                                          month = lubridate::month(date),
                                                          day = lubridate::day(date))

eb_ws_hourly <- eb_ws_hourly %>% mutate(year = lubridate::year(date),
                                                          month = lubridate::month(date),
                                                          day = lubridate::day(date))

c(str(cn_ws_hourly), str(sc_ws_hourly), str(eb_ws_hourly)) 

#cambiamos formato de velocidad del viento (ws)
cn_ws_hourly <- cn_ws_hourly %>%  
  mutate(ws = as.numeric(gsub(",",".", cn_ws_hourly$ws,fixed=TRUE))) # usamos expresiones regulares para cambiar , to . para que as.numeric funcione

sc_ws_hourly <- sc_ws_hourly %>%  
  mutate(ws = as.numeric(gsub(",",".", sc_ws_hourly$ws,fixed=TRUE)))

eb_ws_hourly <- eb_ws_hourly %>%  
  mutate(ws = as.numeric(gsub(",",".", eb_ws_hourly$ws,fixed=TRUE)))

#######################################
######### missing data  ###############
#######################################

library(visdat) # utilizamos la libreria visdat

cn_missing_plot <- visdat::vis_miss(cn_ws_hourly) + ggtitle("Cerro Navia") # cerro navia
sc_missing_plot <- visdat::vis_miss(sc_ws_hourly) + ggtitle("Santiago Centro")# santiago centro
eb_missing_plot <- visdat::vis_miss(eb_ws_hourly) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot_hourly <- cn_missing_plot / sc_missing_plot / eb_missing_plot

# revisamos missing values en nuestra serie de tiempo horaria 
cn_ws_hourly %>%
  is.na() %>%
  colSums() # tenemos 1104 NA

sc_ws_hourly %>%  
  is.na() %>% 
  colSums() # tenemos  503 NA

eb_ws_hourly %>% 
  is.na() %>% 
  colSums() # tenemos 6422 NA 

# revisamos los datos por año 
cn_ws_hourly %>%  select(year, ws) %>% group_by(year) %>% naniar::miss_var_summary() 
sc_ws_hourly %>%  select(year, ws) %>% group_by(year) %>%naniar::miss_var_summary() 
eb_ws_hourly %>%  select(year, ws) %>% group_by(year) %>% naniar::miss_var_summary() 

# revisamos si existen hidden missing values
cn_ws_hourly %>% naniar::miss_scan_count(search = list("NA", "N/A", "na", ""))
sc_ws_hourly %>% naniar::miss_scan_count(search = list("NA", "N/A", "na", ""))
eb_ws_hourly %>% naniar::miss_scan_count(search = list("NA", "N/A", "na", ""))

###############################################
######### calculo datos diarios ###############
###############################################

# calculamos la velocidad del viento promedio, maxima y minima para cada uno de los dias, utilzando los datos horarios

cn_ws_daily <- cn_ws_hourly %>% group_by(date) %>%  # calculo cerro navia
  summarise(ws_mean = mean(ws , na.rm = FALSE),
            ws_max = max(ws, na.rm = TRUE),
            ws_min = min(ws, na.rm = TRUE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


sc_ws_daily <- sc_ws_hourly %>% group_by(date) %>%  # calculo santiago centro
  summarise(ws_mean = mean(ws , na.rm = TRUE),
            ws_max = max(ws, na.rm = TRUE),
            ws_min = min(ws, na.rm = TRUE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))


eb_ws_daily <- eb_ws_hourly %>% group_by(date) %>% # calculo el bosque
  summarise(ws_mean = mean(ws , na.rm = TRUE),
            ws_max = max(ws , na.rm = TRUE),
            ws_min = min(ws , na.rm = TRUE)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))

###################################################
######### revisamos missings values ###############
###################################################

library(visdat) # utilizamos la libreria visdat

cn_missing_plot <- visdat::vis_miss(cn_ws_daily) + ggtitle("Cerro Navia") # cerro navia
sc_missing_plot <- visdat::vis_miss(sc_ws_daily) + ggtitle("Santiago Centro")# santiago centro
eb_missing_plot <- visdat::vis_miss(eb_ws_daily) + ggtitle("El Bosque") # el bosque

library(patchwork)
missing_plot <- cn_missing_plot / sc_missing_plot / eb_missing_plot

# revisamos missing values en nuestra serie de tiempo horaria 
cn_ws_daily %>%
  is.na() %>%
  colSums() # tenemos 56 NA

sc_ws_daily %>%  
  is.na() %>% 
  colSums() # tenemos 17 NA

eb_ws_daily %>% 
  is.na() %>% 
  colSums() # tenemos 258 NA 


# revisamos hidden missing values
cn_ws_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

sc_ws_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

eb_ws_daily %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"))

# reemplazamos por valores perdidos NA

cn_ws_daily <- cn_ws_daily %>% 
  replace_with_na(replace = list(ws_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 ws_max = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 ws_min = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))


sc_ws_daily <- sc_ws_daily %>% 
  replace_with_na(replace = list(ws_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 ws_max = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 ws_min = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

eb_ws_daily <- eb_ws_daily %>% 
  replace_with_na(replace = list(ws_mean = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 ws_max = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf"),
                                 ws_min = c("n/a","na", "N/A", "NA", "NaN", "Inf", "-Inf")))

############################################
######### exportamos los archivos ##########
############################################

rio::export(cn_ws_daily, "cn_ws_daily.xlsx") # exportamos cerro navia

rio::export(sc_ws_daily, "sc_ws_daily.xlsx") # exportamos santiago centro

rio::export(eb_ws_daily, "eb_ws_daily.xlsx") # exportamos el bosque


####################################
######### visualizaciones ##########
####################################

# scatter plot

# generamos un grafico para revisar la serie de tiempo de las velocidades del viento
cn_ws_scatter <- ggplot2::ggplot(cn_ws_daily, mapping = aes(x = date, y = ws_mean)) + 
  geom_point() + xlab("Date") + ylab("Wind Speed") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth() + ggtitile("Cerro Navia") # si queremos añadir la tendencia

sc_ws_scatter <- ggplot2::ggplot(sc_ws_daily, mapping = aes(x = date, y = ws_mean)) + 
  geom_point() + xlab("Date") + ylab("Wind Speed") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth() + ggtitle("Santiago Centro") # si queremos añadir la tendencia


eb_ws_scatter <- ggplot2::ggplot(eb_ws_daily, mapping = aes(x = date, y = ws_mean)) + 
  geom_point() + xlab("Date") + ylab("Wind Speed") + theme_classic() + 
  scale_x_date(date_breaks = '1 years', # definimos cada cuantos años tenemos un break 
               date_labels = "%Y") + # definimos que aparece como etiqueta en este caso %Y indica año
  geom_smooth() + ggtitle("El Bosque") # si queremos añadir la tendencia

cn_ws_scatter / sc_ws_scatter / eb_ws_scatter


