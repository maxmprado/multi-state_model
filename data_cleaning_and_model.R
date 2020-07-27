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
head(datos[c("folioviv","foliohog","id_pobla","anio_retro","anio_nac", "sexo","niv_aprob",
             #"trabajo","pos_tra",
             "edo_civil1","edo_civil2","edo_civil3","edo_civil4","edo_civil5","edo_civil6")])


datos$
# Extraemos de folioviv la residencia del individuo