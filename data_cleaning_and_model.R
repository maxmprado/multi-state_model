library(dplyr)
#modelo multi estado para las relaciones conyugales

# Descargamos los datos directamente del INEGI
archivoTemporal<- tempfile()
zip <-  "https://www.inegi.org.mx/contenidos/programas/eder/2017/microdatos/eder2017_bases_csv.zip"
download.file(zip,archivoTemporal)
datos <- read.csv(unz(archivoTemporal,"historiavida.csv"))
unlink(archivoTemporal)

# Empezamos limpiando los datos


# Crearemos la variable id concatenando 3 variables 
# Creamos la variable residencia del individuo
# Esta información la extraemos de folioviv 
datos <- datos %>% as_tibble()  %>% mutate(id = paste0(datos$folioviv,datos$foliohog,datos$id_pobla),
                                           resid = ifelse(substr(datos$folioviv,3,3)=="6","Rural","Urbana")
                                           )

# Seleccionamos sólo variables necesarias
datos <- datos %>% select(id, anio_retro, anio_nac, resid, sexo,
                          #trabajo, pos_tra,
                          niv_aprob,starts_with("edo_civil"))




# De esta tabla auxiliar extraeremos los estados
# que no son de nuestro interés
y <- datos %>% mutate_at(.vars = vars(edo_civil1:edo_civil6),
            .funs = list(~ ifelse((.>10 & .%%10 != 0) ,(.*0+100),.)))

# Removeremos los ID de las personas cuyos estados
# no son de interés para este análisis
remover <-  unique(y %>% filter(
            edo_civil1==100 |
            edo_civil2==100 |
            edo_civil3==100 |
            edo_civil4==100 |
            edo_civil5==100 |
            edo_civil6==100 ) %>% select(id))

# length(unique(datos$id)) #23831

532/23831 #eliminado entre muestra total

# eliminaremos 532 personas de la muestra
# lo que representa una proporción menor al
# 3% del total de personas en observación
# por lo tanto no afecta significativamente
# la población original.

datos <- datos %>% anti_join(remover)


# Nos interesan únicamente los años en 
# que hubo cambio de estado

# Reacomodamos los datos en un nuevo data frame de manera que tengamos toda la historia
# En una sola columna
View(subset(datos,edo_civil1<10 & edo_civil2<10 & edo_civil3 <10 ))

names(datos) <- c("id","año","nacimiento","residencia","género","escolaridad"
                  ,"edo","edo","edo","edo","edo","edo") #rename

datoss <- rbind(datos[-c(8,9,10,11,12)],datos[-c(7,9,10,11,12)],
                datos[-c(7,8,10,11,12)],datos[-c(7,8,9,11,12)],
                datos[-c(7,8,9,10,12)],datos[-c(7,8,9,10,11)])
datoss <- datos[ ]
View(datoss)
length(unique(datoss[datoss$edo<10 & datos$edo > 0,]$id))
# Removemos los datos inecesarios

arrange(datoss,id,año)
