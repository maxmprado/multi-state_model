library(dplyr)
#modelo multi estado para las relaciones conyugales

# Descargamos los datos directamente del INEGI
archivoTemporal<- tempfile()
zip <-  "https://www.inegi.org.mx/contenidos/programas/eder/2017/microdatos/eder2017_bases_csv.zip"
download.file(zip,archivoTemporal)
datos <- read.csv(unz(archivoTemporal,"historiavida.csv"))
unlink(archivoTemporal)

# Empezamos limpiando los datos
# Seleccionamos sólo variables necesarias
#1 sí, 2 no

datos$ID <- paste0(datos$folioviv,datos$foliohog,datos$id_pobla) #mutate
# Extraemos de folioviv la residencia del individuo
datos$residencia <- ifelse(substr(datos$folioviv,3,3)=="6","Rural","Urbana")  #mutate
datos<-datos[c("ID","anio_retro","anio_nac", "residencia","sexo","niv_aprob",
             #"trabajo","pos_tra",
             "edo_civil1","edo_civil2","edo_civil3","edo_civil4","edo_civil5","edo_civil6")] #select
View(datos)

#Nos interesan únicamente los años en que hubo cambio de estado
#No podemos estudiar a los individuos que tuvieron cambio de estado en el mismo año

unique(datos$ID)
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
#otra prueba