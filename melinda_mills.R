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



# renombramos los nombres para aquellos que terminaron de estudioar
levels(datos$nivel_esc)[startsWith(levels(datos$nivel_esc),"9")] <- 
  substring(levels(datos$nivel_esc)[startsWith(levels(datos$nivel_esc),"9")],2)

# Seleccionamos sólo variables necesarias
datos <- datos %>% select(id, anio_retro, anio_nac, resid, sexo,
                          #trabajo, pos_tra,
                          niv_aprob,nivel_esc, edo_civil1,edo_civil2)


# De esta tabla auxiliar extraeremos los estados
# que no son de nuestro interés
# Removeremos los ID de las personas cuyos estados
# no son de interés para este análisis


remover1 <- (datos %>% mutate_at(.vars = vars(edo_civil1:edo_civil2),
                                 .funs = list(~ ifelse((.>10 & .%%10 != 0) ,(.*0+100),.))) %>% 
               filter(
                   nivel_esc == "&"|
                   edo_civil1==100 |
                   edo_civil2==100 ) %>% select(id) %>% distinct())



datos <- datos %>% anti_join(remover1)

# Nos interesan únicamente los años en 
# que hubo cambio de estado

# Ahora vamos a eliminar a los individuos que tuvieron
# varias parejas al mismo tiempo

edos_rep<-c(1:4,seq(10,40, by=10))

remover2 <- (unique(datos %>% filter(edo_civil1 %in% edos_rep & edo_civil2 %in% edos_rep) %>% select(id)))

datos <- datos %>% anti_join(remover2)


# Aún habiendo eliminado a estos individuos tenemos este porcentaje de la información
22894/23831 
# la info removida representa una proporción menor al
# 4% del total de personas en observación
# por lo tanto no afecta significativamente
# la población original.


# para replicar el modelo de Melinda, nos interesa analizar 
# a las personas que después de su primera separación, divorcio o viudez
# entraron nuevamente a un matrimonio

# Reacomodamos los datos en un nuevo data frame de manera que tengamos toda la historia
# En una sola columna
dd <- (rbind(datos %>% select(1:6,edo=edo_civil1),
             datos %>% mutate(edo_civil2 = if_else(edo_civil2 > 2, 0 ,as.numeric(edo_civil2) )) %>% select(1:6,edo=edo_civil2)
)
%>%distinct()%>% arrange(id, anio_retro)
%>% mutate(edo = ifelse(anio_retro!=anio_nac & edo == 0,100,edo))
%>%filter(edo<10) %>% mutate(edo = case_when(between(edo,2,4)~3,
                                     edo==6~4,
                                     edo==8~4,
                                     edo==0~1,
                                     edo==1~2,
                                     edo==7~5)))

for (k in 1:length(dd$edo)){
  if ( dd$edo[k] == 1 ){
    i=1;j=1
  }
  if(dd$edo[k]==2){
    dd$edo[k]<-dd$edo[k]*i
    i=10
  }
  if(dd$edo[k]==3){
    dd$edo[k]<-dd$edo[k]*j
    j=10
  }
}

dd <- dd%>% filter(edo != 20) %>% mutate(edo = if_else(edo==30,6,as.numeric(edo)),
                                         id=as.factor(id),
                                         anio_retro = as.numeric(anio_retro - anio_nac),
                                         resid = as.factor(resid),
                                         sexo = as.factor(sexo),
                                         #edo = as.factor(edo),
                                         niv_aprob = ifelse(is.na(niv_aprob),0,niv_aprob))


suppressPackageStartupMessages(library(mstate))



edos <- dd %>% spread( key = edo, value = edo) %>%
  select(1,"N" =7, "U"=8, "M" = 9, "D"=10, "S"=11,"MM"=12) %>%
  group_by(id) %>% summarise_all(sum, na.rm = T) %>%
  mutate_at(.vars = vars("U":"MM"),
            .funs = list(~ ifelse(.>0, 1,.)))%>%
  mutate(N = NA) %>%
  select("N":"MM") %>% 
  as.data.frame()

edos<-edos %>% mutate(D = if_else(M==1&S==1,as.numeric(S),as.numeric(D)), 
                   S =if_else(M==1&S==1,0,as.numeric(S))) %>%
            mutate(S = if_else(U==1&M==0&D==1,as.numeric(D),as.numeric(S)),
                   D = if_else(U==1&M==0&D==1,0,as.numeric(D)))


times <- dd %>%
  spread(key = edo, value = anio_retro) %>%
  select(1:5,"tN" =6, "tU"=7, "tM" = 8, "tD"=9, "tS"=10,"tMM"=11) %>%
  group_by(id,anio_nac,resid,sexo,niv_aprob) %>%
  summarise_at(.vars = vars("tN":"tMM"),sum, na.rm = T) %>%
  #mutate_all(~ replace(., . == 0, NA)) %>%
  mutate(tN = NA) %>% 
  as.data.frame() %>%
  select("tN":"tMM") 

times<-times %>% mutate(tD = if_else(tM>0&tS>0,as.numeric(tS),as.numeric(tD)), 
                      tS =if_else(tM>0&tS>0,0,as.numeric(tS))) %>%
  mutate(tS = if_else(tU>0&tM==0&tD>0,as.numeric(tD),as.numeric(tS)),
         tD = if_else(tU>0&tM==0&tD>0,0,as.numeric(tD)))

covariates <- dd %>%
  spread(key = edo, value = anio_retro) %>%
  select(2:5)


View(times %>% filter(tS == tMM))
transListn <- list("N" = c(2, 3), "U" = c(3,5), "M" = c(4), "D"=c(2,6),"S"=c(3,6),"MM"=c())
tmat <- transMat(transListn)

View(times)
msprep(time = times, status = edos ,trans = tmat)
eder.ms <- 



events(eder.ms)
eder.ms2$expand.covs(eder.ms, covs = c("resid"), append = T)
eder.ms2<-expand.covs(eder.ms, names(covariates), append = T, longnames = F)
attach(eder.ms2)
detach(eder.ms2)
eder.ms2$niv_apro

coxph(Surv(Tstart, Tstop, status)~anio_nac+resid+sexo+niv_aprob,
      data = eder.ms2, method =  "breslow")



coxph( ~ dissub1.1 + dissub2.1 +
         + age1.1 + age2.1 + drmatch.1 + tcd.1 + dissub1.2 + dissub2.2 +
         + age1.2 + age2.2 + drmatch.2 + tcd.2 + dissub1.3 + dissub2.3 +
         + age1.3 + age2.3 + drmatch.3 + tcd.3 + strata(trans), data = msbmt,
       + method = "breslow")
class(eder.ms)


c(times[2])
times
dim(edos)
dim(times)
tmat

