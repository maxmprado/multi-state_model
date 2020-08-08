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





# Convertimos a formato largo para poder prepara los datos
# con mstate.

# ddd <- dd %>% spread(key = c("edo"), value = c("anio_retro"))

View(dd)

dd <- (dd %>% mutate(edo = case_when(between(edo,2,4)~3,
                              between(edo,6,7)~4,
                              edo==0~1,
                              edo==1~2,
                              edo==8~5)))

dd <- dd %>% mutate(id=as.factor(id),
                    anio_retro = as.numeric(anio_retro - anio_nac),
                    resid = as.factor(resid),
                    sexo = as.factor(sexo),
                    #edo = as.factor(edo),
                    niv_aprob = ifelse(is.na(niv_aprob),0,niv_aprob))

levels(dd$edo) <- c("N","U","M","R","V")

dd$anio_retro <- dd$anio_retro-dd$anio_nac
# dd$edo <- dd$edo+1
# dd[dd$edo>6,]$edo <- dd[dd$edo>6,]$edo - 1

#library(msm)

suppressPackageStartupMessages(library(mstate))
# matriz de transiciones con transMat

edos <- dd %>% spread( key = edo, value = edo) %>%
               select(1,"N" =7, "U"=8, "M" = 9, "S"=10, "V"=11) %>%
               group_by(id) %>% summarise_all(sum, na.rm = T) %>%
            mutate_at(.vars = vars("U":"V"),
            .funs = list(~ ifelse(.>0, 1,.)))%>%
               mutate(N = NA) %>%
               select("N":"V") %>% 
               as.data.frame()


times <- dd %>%
                spread(key = edo, value = anio_retro) %>%
                select(1:5,"tN" =6, "tU"=7, "tM" = 8, "tS"=9, "tV"=10) %>%
                group_by(id,anio_nac,resid,sexo,niv_aprob) %>%
                summarise_at(.vars = vars("tN":"tV"),sum, na.rm = T) %>%
                #mutate_all(~ replace(., . == 0, NA)) %>%
                mutate(tN = NA) %>% 
                as.data.frame() %>%
                select("tN":"tV")


covariates <- dd %>%
  spread(key = edo, value = anio_retro) %>%
  select(2:5)

dim(edos)
dim(times)
dim(covariates)

edos
times
covariates
ddd <- times %>% inner_join(edos)

transListn <- list("N" = c(2, 3), "U" = c(3,4,5), "M" = c(4,5), "S"=c(),"V"=c())
tmat <- transMat(transListn)

msprep(time=c(NA,"tU","tM", "tS","tV"),
       status=c(NA,"U","M", "S","V"),data=ddd,
       id="id",trans=tmat)

eder.ms <- msprep(time = times, status = edos ,trans = tmat,keep=covariates)

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







dd <-  dd %>% filter(duplicated(id)) %>% select(id)%>%unique() %>% left_join(dd)

length(unique(dd$id))
(23200-18058)/23200
qmat0 <-matrix(c(0,1,1,1,1,0,0,0,
                 0,0,1,1,1,0,1,1,
                 0,0,0,0,0,1,1,1,
                 0,0,0,0,0,1,1,1,
                 0,0,0,0,0,1,1,1,
                 0,1,1,1,1,0,0,0,
                 0,1,1,1,1,0,0,0,
                 0,1,1,1,1,0,0,0),
                       nrow = 8, ncol = 8, byrow=TRUE,
                      dimnames=list(from=1:8,to=1:8))#,
                       #dimnames=list(c(0:4,6:8),c(0:4,6:8))) 



qmat0 <- matrix(c(0,1,1,0,0,
                  0,0,1,1,1,
                  0,0,0,1,1,
                  0,0,0,0,0,
                  0,0,0,0,0),
                nrow = 5, ncol = 5, byrow=TRUE,
                dimnames=list(from=1:5,to=1:5))


statetable.msm(edo,id,data = dd)

qmat1 <- crudeinits.msm(edo~anio_retro, subject = id, data = dd, qmatrix = qmat0)

fit <- msm(edo~anio_retro, subject = id, data = dd  ,qmatrix = qmat1,control=list(fnscale=10000,maxit=500))
fit <- msm(edo~anio_retro, subject = id, data = dd  ,qmatrix = qmat1, control =list(fnscale=80000,reltol = 1e-16))

pearson.msm(fit)
summary(fit)
absorbing.msm(fit, qmatrix = qmat1)

lrtest.msm(fit)
summary.msm(fit)

sojourn.msm(fit)
plot(fit)
pmatrix.msm(fit)

qmat2 <- qmatrix.msm(fit)$estimate


fit2 <- msm(edo~anio_retro, subject = id, data = dd, deathexact = c(4,5),
            qmatrix = qmat2, covariates = ~resid )


hazard.msm(fit2)
pearson.msm(fit2)
plot(fit2)







