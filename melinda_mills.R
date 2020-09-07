suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
#modelo multi estado para las relaciones conyugales

# Descargamos los datos directamente del INEGI
archivoTemporal<- tempfile()
zip <-  "https://www.inegi.org.mx/contenidos/programas/eder/2017/microdatos/eder2017_bases_csv.zip"
download.file(zip,archivoTemporal)
datos <- read.csv(unz(archivoTemporal,"historiavida.csv"))
respaldo <- datos
unlink(archivoTemporal)

# Crearemos la variable id concatenando 3 variables 
# Creamos la variable residencia del individuo
# Esta información la extraemos de folioviv 
datos <- datos  %>% mutate(id = paste0(datos$folioviv,datos$foliohog,datos$id_pobla),
                           resid = ifelse(substr(datos$folioviv,3,3)=="6","Rural","Urbana")
)


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

# para replicar el modelo de Melinda, nos interesa analizar 
# a las personas que después de su primera separación, divorcio o viudez
# entraron nuevamente a un matrimonio

datos <- datos %>% mutate(edo_civil2 = if_else(edo_civil2>4,0,as.numeric(edo_civil2)))


View(dd)      
dd <- (rbind(datos %>% select(1:6,edo=edo_civil1),
             datos %>% select(1:6,edo=edo_civil2)
)
%>%distinct()%>% arrange(id, anio_retro)
%>% mutate(edo = ifelse(anio_retro!=anio_nac & edo == 0,100,edo))
%>%filter(edo<10) %>% mutate(edo = case_when(between(edo,2,4)~3,#matrimonio
                                             between(edo,6,8)~4,#separación
                                             edo==0~1,#nunca unido
                                             edo==1~2)))#unión libre

for (k in 1:length(dd$edo)){
  
  if ( dd$edo[k] == 1 ){
    i=1;j=1
  }
  
  if(dd$edo[k]==2){
    dd$edo[k]<-dd$edo[k]*i
    i=10
  }
  
  if(dd$edo[k] == 3){
    dd$edo[k]<-dd$edo[k]*j
    j=10
  }
  
}

dd <- dd%>% filter(edo != 20) %>% mutate(edo = if_else(edo==30,5,as.numeric(edo)),
                                         id=as.factor(id),
                                         anio_retro = as.numeric(anio_retro - anio_nac),
                                         resid = as.factor(resid),
                                         sexo = as.factor(sexo),
                                         #edo = as.factor(edo),
                                         niv_aprob = ifelse(is.na(niv_aprob),0,niv_aprob))

suppressPackageStartupMessages(library(mstate))
library(msm)
transListn <- list("N" = c(2, 3), "U" = c(3,4,5), "M" = c(4), "S"=c(2,3,5),"MM"=c())
tmat <- transMat(transListn)

edos <- dd %>% spread( key = edo, value = edo) %>%
  select(1,"N" =7, "U"=8, "M" = 9, "S"=10, "MM"=11) %>%
  group_by(id) %>% summarise_all(sum, na.rm = T) %>%
  mutate_at(.vars = vars("U":"MM"),
            .funs = list(~ ifelse(.>0, 1,.)))%>%
  mutate(N = NA) %>% #select(id,"N":"MM") %>%
  as.data.frame()

times <- dd %>%
  spread(key = edo, value = anio_retro) %>%
  select(1:5,"tN" =6, "tU"=7, "tM" = 8, "tS"=9, "tMM"=10) %>%
  group_by(id,anio_nac,resid,sexo,niv_aprob) %>%
  summarise_at(.vars = vars("tN":"tMM"),sum, na.rm = T) %>%
  #mutate_all(~ replace(., . == 0, NA)) %>%
  mutate(tN = NA) %>%
  as.data.frame() #%>% select(id,"tN":"tMM") 




quitar <- times %>% filter(tU>0&tU==tS|tM>0&tM==tS|tS>0&tS==tMM) %>% select(id)

edos <- edos %>% anti_join(quitar) #%>% select("N":"MM")
times <- times %>% anti_join(quitar) #%>% select("tN":"tMM")

ms.data <- times %>% inner_join(edos)

colnames(ms.data) <- c("id","anio_nac","resid", 
                       "sexo","niv_aprob" ,"tN","tU","tM","tS","tMM",NA,
                       "U","M","S","MM")  



options(expressions = 500000)

msprep(time = c(NA,"tU","tM","tS","tMM"), 
       status = c(NA,"U","M","S","MM"),
       data = ms.data,
       start=list(state = 1, time='tN'),
       id = "id",
       trans = tmat)



edos <- edos %>% anti_join(quitar) %>% select("N":"MM")
times <- times %>% anti_join(quitar) %>% select("tN":"tMM")
colnames(edos) <- c("N", "U","M","S","MM")


edos$N <- rep(1,length(edos$N))
times$tN <- rep(0,length(edos$N))

ed <- edos[310:315,]
ti <- times[310:315,]

msprep(time = ti, 
       status = ed,
       start=list(state = 1, time=0),
       trans = tmat)



debugonce(msprep)
