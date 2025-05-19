# OBJETIVO: MANIPULAR DATOS EN FORMATO GTFS PARA HACER ANALISIS GEOGRAFICO DE MOVILIDAD. (IDENTIFICAR LAS ESTACIONES MAS AGLOMERADAS)
# https://cran.r-project.org/web/packages/tidytransit/vignettes/frequency.html

# LIBRERIAS.
library(tidytransit)
library(terra)
library(sp)
library(sf)
library(dplyr)

# RUTA AL DIRECTORIO DE TRABAJO.
PATH = "C:/proyecto_ep/"

# ESTABLECER DIRECTORIO DE TRABAJO.
setwd(PATH)
getwd() # VER EL DIRECTORIO

# ver contendo del directorio PATH.
dir()

# Especifica la ruta al archivo GTFS (puede ser un archivo .zip o un directorio).
# Lee el archivo GTFS
gtfs_data = read_gtfs('gtfs.zip')

# EXAMINAR LA ESTRUCTURA DEL ARCHIVO.
str(gtfs_data)
names(gtfs_data)
summary(gtfs_data)

## FRECUENCIAS ABSOLUTAS PARA IDENTIFICAR LAS PARADAS MAS TRANSITADAS.

a = (gtfs_data$stop_times$stop_id)
b = (gtfs_data$stops$stop_id)

# localizar todos los registros comunes.
intersect(a, b)
# localizar los regsitros que se encuentran en el catalogo de stops pero no se encuentran en stop times.
setdiff(b, a)

# volver nuestro tibble a dataframe para evitar problemas al graficar.
stops = as.data.frame(gtfs_data$stops)
head(stops)

# creamos un vector el cual contiene las paradas mas frecuentdas.
freq_stops = table(a)
head(freq_stops)
str(freq_stops)

# ordenamos los registros de mayor a menor frecuencia.
A = c(4,5,3)
A[c(3,1,2)]

freq_stops_ord = freq_stops[order(freq_stops,decreasing = TRUE)]
names(freq_stops_ord)

# creamos un vector que contenga nuestros stop_id transformados de chr a Factor.
stops$stop_id = factor(stops$stop_id, levels = names(freq_stops_ord))
str(stops)

# Ya que logramos trasformarlas a variables ordinales le indicamos a nuesta terminal que ordene nuestros id.
stops = stops[order(stops$stop_id),]

# creamos una variable que contenga los 20 registros con mayor aglomeracion.
top_freq_stop_id = head(freq_stops_ord,20)
names(top_freq_stop_id)
stops[1:10,1]

# Con la funcion de which transformamos los registros logicos a indices numericos.
which(stops$stop_id %in% names(top_freq_stop_id))
str(top_freq_stop_id)

# creamos un vector llamado names el cual contnga la lista de stop names ordenados de mayor a menor para nuestra grafica.
nombres = stops[stops$stop_id %in% names(top_freq_stop_id),"stop_name"]
str(nombres)
names(top_freq_stop_id)

# Editando los parametros ('par') del grafico de barras del top 20 de paradas mas aglomeradas. 
# mar = c(abajo, izquierda, arriba, derecha): Este ARGUMENTO especifica el tamaño de los márgenes en líneas de texto. 

par(mar = c(16,3,3,3))

# Gafica de barras
barplot(top_freq_stop_id, las = 2,names.arg = nombres)

# TRANSFORMACION DE DATAFRAME A ARCHIVO SHP.
catalogo_completo_stops = as.data.frame(gtfs_data$stops)
df_freq_stops = as.data.frame(freq_stops)
str(df_freq_stops)
catalogo_filtrado_con_frecuencia = merge(catalogo_completo_stops, df_freq_stops, by.x = "stop_id", by.y = "a")
catalogo_stop_join_stop_times = merge(catalogo_filtrado_con_frecuencia,gtfs_data$stop_times, by = "stop_id")

# con la funcion str() rectificamos que se encuentre la columna 'trip_id' tanto en catalogo_stop_join_stop_times como en gtfs_data$trip. 
# by = "trip_id" ES NUESTRO ARGUMENTO LLAVE PARA REALIZAR EL JOIN

str(catalogo_stop_join_stop_times)
gtfs_data$trips

stop_trips = merge(catalogo_stop_join_stop_times, gtfs_data$trips[,c(3,6)] ,by = "trip_id")

head(stop_trips)
str(stop_trips)

# ARCHIVO SHP #converting gtfs to .shp 

shapes_sf = shapes_as_sf(gtfs_shapes = gtfs_data$shapes)
plot(shapes_sf)
help(shapes_as_sf) 

df_shapes_stop_trips = merge(shapes_sf, stop_trips, by = "shape_id")
class(df_shapes_stop_trips)
str(df_shapes_stop_trips)

st_write(df_shapes_stop_trips, "C:/proyecto_ep/stops_trips")
