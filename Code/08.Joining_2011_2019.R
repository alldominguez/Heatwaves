# Combinar los datos 2011-2019 

#librerias 
pacman::p_load(tidyverse, lubridate, heatwaveR, readxl, rio, naniar, visdat, mice, patchwork)

getwd() #revisamos el espacio de trabajo

setwd("/Users/aldominguez/Desktop/Github_Proyectos/Heatwaves/Data") # asignamos nuestro espacio de trabajo
       
#######################################
########### Cerro Navia ###############
#######################################

### cargamos los datos 2017 - 2019 ###

cn_2017_2019_all <- read_xlsx("SINCA_DEIS_2017_2019/cn_all_causes_sinca_2017_2019.xlsx")

cn_2017_2019_cardio <- read_xlsx("SINCA_DEIS_2017_2019/cn_cardio_sinca_2017_2019.xlsx")

cn_2017_2019_resp <- read_xlsx("SINCA_DEIS_2017_2019/cn_resp_sinca_2017_2019.xlsx") 

c(str(cn_2017_2019_all), str(cn_2017_2019_cardio), str(cn_2017_2019_resp))

colnames(cn_2017_2019_all)

### cargamos los datos 2011 - 2016 ###

cn_2011_2016_all <- read_xlsx("SINCA_DEIS_2011_2016/cn_all_causes_sinca_2011_2016.xlsx")

cn_2011_2016_cardio <- read_xlsx("SINCA_DEIS_2011_2016/cn_cardio_sinca_2011_2016.xlsx")

cn_2011_2016_resp <- read_xlsx("SINCA_DEIS_2011_2016/cn_resp_sinca_2011_2016.xlsx") 

c(str(cn_2011_2016_all), str(cn_201_2016_cardio), str(cn_2011_2016_resp))

colnames(cn_2011_2016_all)

############# RENAME #############

# modificamos el nombre de las variables, para poder unirlas 

# all causes 
colnames(cn_2011_2016_all)
colnames(cn_2017_2019_all)

cn_2011_2016_all <- cn_2011_2016_all %>% 
  rename(temp_mean = 'Tprom', temp_max = 'Tmax', temp_min = 'Tmin', #temperature
         pm25_mean = 'MP2.5', pm10_mean = 'MP10', o3_mean = 'O3',  # material particulado y o3
         ws_mean = 'Vvprom', ws_max = 'VV max', ws_min = 'Vvmin', #  velocidad del viento
         rh_mean = 'Hrprom', rh_max = 'Hrmax', rh_min = 'Hrmin') # humedad relativa

# cardiovascular 
colnames(cn_2011_2016_cardio)
colnames(cn_2017_2019_cardio)

cn_2011_2016_cardio <- cn_2011_2016_cardio %>% 
  rename(temp_mean = 'Tprom', temp_max = 'Tmax', temp_min = 'Tmin', #temperature
         pm25_mean = 'MP2.5', pm10_mean = 'MP10', o3_mean = 'O3',  # material particulado y o3
         ws_mean = 'Vvprom', ws_max = 'Vvmax', ws_min = 'Vvmin', #  velocidad del viento
         rh_mean = 'Hrprom', rh_max = 'Hrmax', rh_min = 'Hrmin') # humedad relativa


# respiratory
colnames(cn_2011_2016_resp)
colnames(cn_2017_2019_resp)

cn_2011_2016_resp <- cn_2011_2016_resp %>% 
  rename(temp_mean = 'Tprom', temp_max = 'Tmax', temp_min = 'Tmin', #temperature
         pm25_mean = 'MP2.5', pm10_mean = 'MP10', o3_mean = 'O3',  # material particulado y o3
         ws_mean = 'Vvprom', ws_max = 'vvmax', ws_min = 'Vvmin', #  velocidad del viento
         rh_mean = 'Hrprom', rh_max = 'Hrmax', rh_min = 'Hrmin') # humedad relativa

###### unimos las bases ######

cn_2011_2019_all <- dplyr::bind_rows(cn_2011_2016_all, cn_2017_2019_all) # unimos 2011 - 2019 all casuses
cn_2011_2019_cardio <- dplyr::bind_rows(cn_2011_2016_cardio, cn_2017_2019_cardio) # unimos 2011 - 2019 cardiovascular
cn_2011_2019_resp <- dplyr::bind_rows(cn_2011_2016_resp, cn_2017_2019_all) # unimos 2011 - 2019 respiratory

#############################################
############## exportamos ################### #### cerro navia ####
############################################

rio::export(cn_2011_2019_all, "cn_2011_2019_all.xlsx") # exportamos mortalidad por todas las causas
rio::export(cn_2011_2019_cardio, "cn_2011_2019_cardio.xlsx") # exportamos mortalidad por causas cardiovasculares
rio::export(cn_2011_2019_resp, "cn_2011_2019_resp.xlsx") # exportamos mortalidad por causas respiratorias


#######################################
########### El Bosque #################
#######################################

### cargamos los datos 2017 - 2019 ###

eb_2017_2019_all <- read_xlsx("SINCA_DEIS_2017_2019/eb_all_causes_sinca_2017_2019.xlsx")

eb_2017_2019_cardio <- read_xlsx("SINCA_DEIS_2017_2019/eb_cardio_sinca_2017_2019.xlsx")

eb_2017_2019_resp <- read_xlsx("SINCA_DEIS_2017_2019/eb_resp_sinca_2017_2019.xlsx") 

c(str(eb_2017_2019_all), str(eb_2017_2019_cardio), str(eb_2017_2019_resp))

colnames(eb_2017_2019_all)

### cargamos los datos 2011 - 2016 ###

eb_2011_2016_all <- read_xlsx("SINCA_DEIS_2011_2016/eb_all_causes_sinca_2011_2016.xlsx")

eb_2011_2016_cardio <- read_xlsx("SINCA_DEIS_2011_2016/eb_cardio_sinca_2011_2016.xlsx")

eb_2011_2016_resp <- read_xlsx("SINCA_DEIS_2011_2016/eb_resp_sinca_2011_2016.xlsx") 

c(str(eb_2011_2016_all), str(eb_201_2016_cardio), str(eb_2011_2016_resp))

colnames(eb_2011_2016_all)

############# RENAME #############

# modificamos el nombre de las variables, para poder unirlas 

# all causes 
colnames(eb_2011_2016_all)
colnames(eb_2017_2019_all)

eb_2011_2016_all <- eb_2011_2016_all %>% 
  rename(temp_mean = 'Tprom', temp_max = 'Tmax', temp_min = 'Tmin', #temperature
         pm25_mean = 'MP2.5', pm10_mean = 'MP10', o3_mean = 'O3',  # material particulado y o3
         ws_mean = 'Vvprom', ws_max = 'VVmax', ws_min = 'Vvmin', #  velocidad del viento
         rh_mean = 'Hrprom', rh_max = 'Hrmax', rh_min = 'Hrmin') # humedad relativa

# cardiovascular 
colnames(eb_2011_2016_cardio)
colnames(eb_2017_2019_cardio)

eb_2011_2016_cardio <- eb_2011_2016_cardio %>% 
  rename(temp_mean = 'Tprom', temp_max = 'Tmax', temp_min = 'Tmin', #temperature
         pm25_mean = 'MP2.5', pm10_mean = 'MP10', o3_mean = 'O3',  # material particulado y o3
         ws_mean = 'Vvprom', ws_max = 'VVmax', ws_min = 'Vvmin', #  velocidad del viento
         rh_mean = 'Hrprom', rh_max = 'Hrmax', rh_min = 'Hrmin') # humedad relativa


# respiratory
colnames(eb_2011_2016_resp)
colnames(eb_2017_2019_resp)

eb_2011_2016_resp <- eb_2011_2016_resp %>% 
  rename(temp_mean = 'Tprom', temp_max = 'Tmax', temp_min = 'Tmin', #temperature
         pm25_mean = 'MP2.5', pm10_mean = 'MP10', o3_mean = 'O3',  # material particulado y o3
         ws_mean = 'Vvprom', ws_max = 'VVmax', ws_min = 'Vvmin', #  velocidad del viento
         rh_mean = 'Hrprom', rh_max = 'Hrmax', rh_min = 'Hrmin') # humedad relativa


###### unimos las bases ######

eb_2011_2019_all <- dplyr::bind_rows(eb_2011_2016_all, eb_2017_2019_all) # unimos 2011 - 2019 all casuses
eb_2011_2019_cardio <- dplyr::bind_rows(eb_2011_2016_cardio, eb_2017_2019_cardio) # unimos 2011 - 2019 cardiovascular
eb_2011_2019_resp <- dplyr::bind_rows(eb_2011_2016_resp, eb_2017_2019_all) # unimos 2011 - 2019 respiratory

#############################################
############## exportamos ################### #### el bosque ####
############################################

rio::export(eb_2011_2019_all, "eb_2011_2019_all.xlsx") # exportamos mortalidad por todas las causas
rio::export(eb_2011_2019_cardio, "eb_2011_2019_cardio.xlsx") # exportamos mortalidad por causas cardiovasculares
rio::export(eb_2011_2019_resp, "eb_2011_2019_resp.xlsx") # exportamos mortalidad por causas respiratorias


#############################################
########### Santiago Centro #################
##############################################

### cargamos los datos 2017 - 2019 ###

sc_2017_2019_all <- read_xlsx("SINCA_DEIS_2017_2019/sc_all_causes_sinca_2017_2019.xlsx")

sc_2017_2019_cardio <- read_xlsx("SINCA_DEIS_2017_2019/sc_cardio_sinca_2017_2019.xlsx")

sc_2017_2019_resp <- read_xlsx("SINCA_DEIS_2017_2019/sc_resp_sinca_2017_2019.xlsx") 

c(str(sc_2017_2019_all), str(sc_2017_2019_cardio), str(sc_2017_2019_resp))

colnames(sc_2017_2019_all)

### cargamos los datos 2011 - 2016 ###

sc_2011_2016_all <- read_xlsx("SINCA_DEIS_2011_2016/sc_all_causes_sinca_2011_2016.xlsx")

sc_2011_2016_cardio <- read_xlsx("SINCA_DEIS_2011_2016/sc_cardio_sinca_2011_2016.xlsx")

sc_2011_2016_resp <- read_xlsx("SINCA_DEIS_2011_2016/sc_resp_sinca_2011_2016.xlsx") 

c(str(sc_2011_2016_all), str(sc_201_2016_cardio), str(sc_2011_2016_resp))

colnames(sc_2011_2016_all)

############# RENAME #############

# modificamos el nombre de las variables, para poder unirlas 

# all causes 
colnames(sc_2011_2016_all)
colnames(sc_2017_2019_all)

sc_2011_2016_all <- sc_2011_2016_all %>% 
  rename(temp_mean = 'Tprom', temp_max = 'Tmax', temp_min = 'Tmin', #temperature
         pm25_mean = 'PM2.5', pm10_mean = 'PM10', o3_mean = 'ozono',  # material particulado y o3
         ws_mean = 'VV_prom', ws_max = 'VV_max', ws_min = 'VV_min', #  velocidad del viento
         rh_mean = 'HR_prom', rh_max = 'HR_max', rh_min = 'HR_min') # humedad relativa

# cardiovascular 
colnames(sc_2011_2016_cardio)
colnames(sc_2017_2019_cardio)

sc_2011_2016_cardio <- sc_2011_2016_cardio %>% 
  rename(temp_mean = 'Tprom', temp_max = 'Tmax', temp_min = 'Tmin', #temperature
         pm25_mean = 'PM2.5', pm10_mean = 'PM10', o3_mean = 'ozono',  # material particulado y o3
         ws_mean = 'VV_prom', ws_max = 'VV_max', ws_min = 'VV_min', #  velocidad del viento
         rh_mean = 'HR_prom', rh_max = 'HR_max', rh_min = 'HR_min') # humedad relativa

# respiratory
colnames(eb_2011_2016_resp)
colnames(eb_2017_2019_resp)

sc_2011_2016_resp <- sc_2011_2016_resp %>% 
  rename(temp_mean = 'Tprom', temp_max = 'Tmax', temp_min = 'Tmin', #temperature
         pm25_mean = 'PM2.5', pm10_mean = 'PM10', o3_mean = 'ozono',  # material particulado y o3
         ws_mean = 'VV_prom', ws_max = 'VV_max', ws_min = 'VV_min', #  velocidad del viento
         rh_mean = 'HR_prom', rh_max = 'HR_max', rh_min = 'HR_min') # humedad relativa


###### unimos las bases ######

sc_2011_2019_all <- dplyr::bind_rows(sc_2011_2016_all, sc_2017_2019_all) # unimos 2011 - 2019 all casuses
sc_2011_2019_cardio <- dplyr::bind_rows(sc_2011_2016_cardio, sc_2017_2019_cardio) # unimos 2011 - 2019 cardiovascular
sc_2011_2019_resp <- dplyr::bind_rows(sc_2011_2016_resp, sc_2017_2019_all) # unimos 2011 - 2019 respiratory


#############################################
############## exportamos ################### #### el bosque ####
############################################

rio::export(sc_2011_2019_all, "sc_2011_2019_all.xlsx") # exportamos mortalidad por todas las causas
rio::export(sc_2011_2019_cardio, "sc_2011_2019_cardio.xlsx") # exportamos mortalidad por causas cardiovasculares
rio::export(sc_2011_2019_resp, "sc_2011_2019_resp.xlsx") # exportamos mortalidad por causas respiratorias



















