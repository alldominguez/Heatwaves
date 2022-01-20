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

cn_2017_2019_resp <- read_xlsx("SINCA_DEIS_2017_2019/cn_resp_2017_2019.xlsx") 

c(str(cn_2017_2019_all), str(cn_2017_2019_cardio), str(cn_2017_2019_resp))

colnames(cn_2017_2019_all)

### cargamos los datos 2011 - 2016 ###

cn_2011_2016_all <- read_xlsx("SINCA_DEIS_2011_2016/cn_all_causes_sinca_2011_2016.xlsx")

cn_2011_2016_cardio <- read_xlsx("SINCA_DEIS_2011_2016/cn_cardio_sinca_2011_2016.xlsx")

cn_2011_2016_resp <- read_xlsx("SINCA_DEIS_2011_2016/cn_resp_2011_2016.xlsx") 

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












#############################################
########### Santiago Centro #################
############################################
