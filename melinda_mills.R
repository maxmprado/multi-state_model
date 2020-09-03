suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
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
datos <- datos  %>% mutate(id = paste0(datos$folioviv,datos$foliohog,datos$id_pobla),
                           resid = ifelse(substr(datos$folioviv,3,3)=="6","Rural","Urbana")
)

# Seleccionamos sólo variables necesarias
datos <- datos %>% select(id, anio_retro, anio_nac, resid, sexo,
                          #trabajo, pos_tra,
                          niv_aprob,starts_with("edo_civil"))




# De esta tabla auxiliar extraeremos los estados
# que no son de nuestro interés
# Removeremos los ID de las personas cuyos estados
# no son de interés para este análisis

remover1 <- (datos %>% mutate_at(.vars = vars(edo_civil1:edo_civil6),
                                 .funs = list(~ ifelse((.>10 & .%%10 != 0) ,(.*0+100),.))) %>% 
               filter(
                 edo_civil1==100 |
                   edo_civil2==100 |
                   edo_civil3==100 |
                   edo_civil4==100 |
                   edo_civil5==100 |
                   edo_civil6==100 ) %>% select(id) %>% distinct())


datos <- datos %>% anti_join(remover1)

# Nos interesan únicamente los años en 
# que hubo cambio de estado

# Ahora vamos a eliminar a los individuos que tuvieron
# varias parejas al mismo tiempo

edos_rep<-c(1:4,seq(10,40, by=10))

remover2 <- (unique(datos %>% filter((edo_civil1 %in% edos_rep & edo_civil2 %in% edos_rep)|
                                       (edo_civil1 %in% edos_rep & edo_civil3 %in% edos_rep)|
                                       (edo_civil1 %in% edos_rep & edo_civil4 %in% edos_rep)|
                                       (edo_civil2 %in% edos_rep & edo_civil3 %in% edos_rep)|
                                       (edo_civil2 %in% edos_rep & edo_civil4 %in% edos_rep)
) %>% select(id)))


datos <- datos %>% anti_join(remover2)

# En total eliminamos 631 personas de la muestra
(532+99)/23831 
# lo que representa una proporción menor al
# 3% del total de personas en observación
# por lo tanto no afecta significativamente
# la población original.


# Reacomodamos los datos en un nuevo data frame de manera que tengamos toda la historia
# En una sola columna
dd <- (rbind(datos %>% select(1:6,edo=edo_civil1),
             datos %>% select(1:6,edo=edo_civil2),
             datos %>% select(1:6,edo=edo_civil3),
             datos %>% select(1:6,edo=edo_civil4),
             datos %>% select(1:6,edo=edo_civil5),
             datos %>% select(1:6,edo=edo_civil6)
)
%>%distinct()%>% arrange(id, anio_retro)
%>% mutate(edo = ifelse(anio_retro!=anio_nac & edo == 0,100,edo))
%>%filter(edo<10) 
)


dd <- (datos %>% select(1:6,edo=edo_civil1)
       %>%distinct()%>% arrange(id, anio_retro)
       %>% mutate(edo = ifelse(anio_retro!=anio_nac & edo == 0,100,edo))
       %>%filter(edo<10) 
)