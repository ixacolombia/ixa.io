# Este codigo puede tardar mas de 5 minutos en el proceso
library(tidyverse) # analisis de datos
library(RSocrata)  # importar datos en linea
library(dygraphs)  # graficos
library(xts)       #
library(incidence)  # 
library(aTSA)
library(lmtest)
library(forecast)   # modelos de pronosticos
library(dplyr)      # manejo de datos
library(seastests)
library(trend)      

library(flexdashboard)  # tableros
library(rnaturalearth)   # mapas
library(rnaturalearthdata) # mapas
library(plotly)     # graficos dinamicos
library(tidyr)   
library(DT)   # tablas

#------------------------------------------------------------------------------
# baja datos covid colombia desde pagina de Datos Abiertos Colombia
token ="ew2rEMuESuzWPqMkyPfOSGJgE"
Colombia= read.socrata("https://www.datos.gov.co/resource/gt2j-8ykr.json", app_token = token)

 
Colombia$sexo=str_to_lower(Colombia$sexo)
Colombia$estado=str_to_lower(Colombia$estado)
Colombia$recuperado=str_to_lower(Colombia$recuperado)
Colombia$fuente_tipo_contagio=str_to_lower(Colombia$fuente_tipo_contagio)
Colombia$ubicacion=str_to_lower(Colombia$ubicacion)

Colombia$estado[Colombia$estado=="n/a"]="NA"
Colombia$recuperado[Colombia$recuperado=="n/a"]="NA"
Colombia$fuente_tipo_contagio[Colombia$fuente_tipo_contagio=="n/a"]="NA"
Colombia$ubicacion[Colombia$ubicacion=="n/a"]="NA"
# saveRDS(Colombia,"Colombia.RDS")
#-------------------------------------------------------------------------------
# table(Colombia$unidad_medida) # ok
# table(Colombia$sexo)          # ok
# table(Colombia$fuente_tipo_contagio) # ok
# table(Colombia$ubicacion)
# table(Colombia$estado)
# table(Colombia$pais_viajo_1_cod)
# table(Colombia$pais_viajo_1_nom)
# table(Colombia$recuperado)
# table(Colombia$tipo_recuperacion)
# table(Colombia$per_etn_)
# table(Colombia$nom_grupo_)
#-----------------------------------------------------------------------------
Colombia$fecha_reporte_web <- as.Date(Colombia$fecha_reporte_web, tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))
Colombia$fecha_de_notificaci_n <- as.Date(Colombia$fecha_de_notificaci_n, tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))
Colombia$fecha_inicio_sintomas <- as.Date(Colombia$fecha_inicio_sintomas,tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))
Colombia$fecha_diagnostico  <- as.Date(Colombia$fecha_diagnostico, tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))
Colombia$fecha_recuperado <- as.Date(Colombia$fecha_recuperado, tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))
Colombia$fecha_muerte <- as.Date(Colombia$fecha_muerte, tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))
#------------------------------------------------------------------------------
#De la 11 a la 16 convierte en formato fecha corta.
Colombia$confirmados <- "Confirmados" #Crea una columna para tener un conteo de los confirmados diarios.

Colombia <- Colombia %>%
  dplyr::select(id_de_caso,
                ciudad_municipio_nom,
                fecha_inicio_sintomas,
                confirmados,
                fecha_de_notificaci_n,
                fecha_diagnostico,
                fecha_muerte,
                fecha_reporte_web,
                everything()) #Reorganiza la base.


#-------------------------------------------------------------------------------

Colombia$fallecido=as.numeric(Colombia$estado=="fallecido")
Colombia$grave=as.numeric(Colombia$estado=="grave")
Colombia$leve=as.numeric(Colombia$estado=="leve")
Colombia$moderado=as.numeric(Colombia$estado=="moderado")

Colombia$fallecido[Colombia$fallecido==1]="fallecido"
Colombia$fallecido[Colombia$fallecido==0]=NA

Colombia$grave[Colombia$grave==1]="grave"
Colombia$grave[Colombia$grave==0]=NA

Colombia$leve[Colombia$leve==1]="leve"
Colombia$leve[Colombia$leve==0]=NA

Colombia$moderado[Colombia$moderado==1]="moderado"
Colombia$moderado[Colombia$moderado==0]=NA

#----------------------------------------------------------
#########################################################
# https://libretilla.com/ciudades-mas-grandes-de-colombia-por-poblacion/
# Cali - 76001
Colombia$Cali=as.numeric(Colombia$ciudad_municipio==76001)
Cali=Colombia[Colombia$Cali==1,]
#########################################################
# Bogota -  11001
Colombia$Bogota=as.numeric(Colombia$ciudad_municipio==11001)
Bogota=Colombia[Colombia$Bogota==1,]
################################################################################
# Medellin - 05001--------------------------------------------------------------
Colombia$Medellin=as.numeric(Colombia$ciudad_municipio==5001)
Medellin=Colombia[Colombia$Medellin==1,]
################################################################################
# Barranquilla - 08001----------------------------------------------------------
Colombia$Barranquilla=as.numeric(Colombia$ciudad_municipio==8001)
Barranquilla=Colombia[Colombia$Barranquilla==1,]
################################################################################
# Cartagena  13001--------------------------------------------------------------
Colombia$Cartagena=as.numeric(Colombia$ciudad_municipio==13001)
Cartagena=Colombia[Colombia$Cartagena==1,]
################################################################################
# Santa Marta - 47001 ----------------------------------------------------------
Colombia$SantaMarta=as.numeric(Colombia$ciudad_municipio==47001)
SantaMarta=Colombia[Colombia$SantaMarta==1,]
################################################################################
# Bucaramanga -68001------------------------------------------------------------
Colombia$Bucaramanga=as.numeric(Colombia$ciudad_municipio==68001)
Bucaramanga=Colombia[Colombia$Bucaramanga==1,]
################################################################################
# Cucuta - 54001----------------------------------------------------------------
Colombia$Cucuta=as.numeric(Colombia$ciudad_municipio==54001)
Cucuta=Colombia[Colombia$Cucuta==1,]
################################################################################
# Manizales 17001---------------------------------------------------------------
Colombia$Manizales=as.numeric(Colombia$ciudad_municipio==17001)
Manizales=Colombia[Colombia$Manizales==1,]
################################################################################
# Pereira  66001----------------------------------------------------------------
Colombia$Pereira=as.numeric(Colombia$ciudad_municipio==66001)
Pereira=Colombia[Colombia$Pereira==1,]
################################################################################
# Armenia - 63001---------------------------------------------------------------
Colombia$Armenia=as.numeric(Colombia$ciudad_municipio==63001)
Armenia=Colombia[Colombia$Armenia==1,]
################################################################################
# Pasto - 52001-----------------------------------------------------------------
Colombia$Pasto=as.numeric(Colombia$ciudad_municipio==52001)
Pasto=Colombia[Colombia$Pasto==1,]
################################################################################
# Popayan 19001-----------------------------------------------------------------
Colombia$Popayan=as.numeric(Colombia$ciudad_municipio==19001)
Popayan=Colombia[Colombia$Popayan==1,]
################################################################################
# Ibague 73001------------------------------------------------------------------
Colombia$Ibague=as.numeric(Colombia$ciudad_municipio==73001)
Ibague=Colombia[Colombia$Ibague==1,]
################################################################################
# Soledad - 08758 --------------------------------------------------------------
Colombia$Soledad=as.numeric(Colombia$ciudad_municipio==08758)
Soledad=Colombia[Colombia$Soledad==1,]
################################################################################
# Soacha 25754------------------------------------------------------------------
Colombia$Sohacha=as.numeric(Colombia$ciudad_municipio==25754)
Sohacha=Colombia[Colombia$Sohacha==1,]
################################################################################
# Sicelejo - 70001--------------------------------------------------------------
Colombia$Sincelejo=as.numeric(Colombia$ciudad_municipio==70001)
Sincelejo=Colombia[Colombia$Sincelejo==1,]
################################################################################
# Villavicencio  50001---------------------------------------------------------
Colombia$Villavicencio=as.numeric(Colombia$ciudad_municipio==50001)
Villavicencio=Colombia[Colombia$Villavicencio==1,]
################################################################################
# Valledupar  20001-------------------------------------------------------------
Colombia$Valledupar=as.numeric(Colombia$ciudad_municipio==20001)
Valledupar=Colombia[Colombia$Valledupar==1,]
################################################################################
# Monteria  - 23001-------------------------------------------------------------
Colombia$Monteria=as.numeric(Colombia$ciudad_municipio==23001)
Monteria=Colombia[Colombia$Monteria==1,]
################################################################################
# Bello - 05088-----------------------------------------------------------------
Colombia$Bello=as.numeric(Colombia$ciudad_municipio==05088)
Bello=Colombia[Colombia$Bello==1,]
################################################################################
# Tunja 15001-------------------------------------------------------------------
Colombia$Tunja=as.numeric(Colombia$ciudad_municipio==15001)
Tunja=Colombia[Colombia$Tunja==1,]
################################################################################
# Quibdo - 27001----------------------------------------------------------------
Colombia$Quibdo=as.numeric(Colombia$ciudad_municipio==27001)
Quibdo=Colombia[Colombia$Quibdo==1,]
################################################################################
# Neiva - 41001-----------------------------------------------------------------
Colombia$Neiva=as.numeric(Colombia$ciudad_municipio==41001)
Neiva=Colombia[Colombia$Neiva==1,]
################################################################################
# Riohacha - 44001--------------------------------------------------------------
Colombia$Riohacha=as.numeric(Colombia$ciudad_municipio==44001)
Riohacha=Colombia[Colombia$Riohacha==1,]
################################################################################
# Buenaventura 76109------------------------------------------------------------
Colombia$Buenaventura=as.numeric(Colombia$ciudad_municipio==76109)
Buenaventura=Colombia[Colombia$Buenaventura==1,]
################################################################################
# Palmira 76520-----------------------------------------------------------------
Colombia$Palmira=as.numeric(Colombia$ciudad_municipio==76520)
Palmira=Colombia[Colombia$Palmira==1,]
################################################################################
# Yumbo  76892 -----------------------------------------------------------------
Colombia$Yumbo=as.numeric(Colombia$ciudad_municipio==76892)
Yumbo=Colombia[Colombia$Yumbo==1,]
################################################################################
# Jamundi  76364----------------------------------------------------------------
Colombia$Jamundi=as.numeric(Colombia$ciudad_municipio==76364)
Jamundi=Colombia[Colombia$Jamundi==1,]
################################################################################
saveRDS(Colombia, "data/Colombia.RDS")
saveRDS(Cali, "data/Cali.RDS")
saveRDS(Armenia, "data/Armenia.RDS")
saveRDS(Barranquilla, "data/Barranquilla.RDS")
saveRDS(Bogota, "data/Bogota.RDS")
saveRDS(Bucaramanga, "data/Bucaramanga.RDS")
saveRDS(Buenaventura, "data/Buenaventura.RDS")
saveRDS(Cartagena, "data/Cartagena.RDS")
saveRDS(Cucuta, "data/Cucuta.RDS")
saveRDS(Ibague, "data/Ibague.RDS")
saveRDS(Jamundi, "data/Jamundi.RDS")
saveRDS(Manizales, "data/Manizales.RDS")
saveRDS(Medellin, "data/Medellin.RDS")
saveRDS(Monteria, "data/Monteria.RDS")
saveRDS(Neiva, "data/Neiva.RDS")
saveRDS(Palmira, "data/Palmira.RDS")
saveRDS(Pasto, "data/Pasto.RDS")
saveRDS(Pereira, "data/Pereira.RDS")
saveRDS(Popayan, "data/Popayan.RDS")
saveRDS(Riohacha, "data/Riohacha.RDS")
saveRDS(SantaMarta, "data/SantaMarta.RDS")
saveRDS(Sincelejo, "data/Sincelejo.RDS")
saveRDS(Sohacha, "data/Sohacha.RDS")
saveRDS(Soledad, "data/Soledad.RDS")
saveRDS(Valledupar, "data/Valledupar.RDS")
saveRDS(Villavicencio, "data/Villavicencio.RDS")
#-------------------------------------------------------------------------------








