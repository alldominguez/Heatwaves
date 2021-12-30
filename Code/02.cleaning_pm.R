# Datos series de tiempo (2017-2019) - promedios diarios con datos horarios (material particulado)

#librerias 
pacman::p_load(tidyverse, lubridate, heatwaveR, readxl)


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






###################### PM25 ######################



