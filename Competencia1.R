#Aplicacion de los mejores hiperparametros encontrados en una bayesiana
#Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")


#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/home/dao")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./competencia1_2022.csv" )

#feature engineering

dataset[ , propia_mcuenta :=  mcuentas_saldo+mcuenta_corriente ]
dataset[ , propia_mcomisiones :=  mcomisiones+mcomisiones_mantenimiento+mcomisiones_otras ]
dataset[ , propia_ccomisiones :=  ccomisiones_otras+ccomisiones_mantenimiento ]
dataset[ , propia_limite_tarjeta :=  Master_mfinanciacion_limite+Visa_mfinanciacion_limite ]
# dataset[ , propia_msaldototal :=  Master_msaldototal+Visa_msaldototal ]
# dataset[ , propia_msaldopesos :=  Master_msaldopesos+Visa_msaldopesos ]
# dataset[ , propia_msaldodolares :=  Master_msaldodolares+Visa_msaldodolares ]
# dataset[ , propia_mconsumosdolares :=  Master_mconsumosdolares+Visa_mconsumosdolares ]
# dataset[ , propia_msaldototal :=  Master_msaldototal+Visa_msaldototal ]
# dataset[ , propia_mconsumototal :=  Master_mconsumototal+Visa_mconsumototal ]
# dataset[ , propia_mconsumospesos :=  Master_mconsumospesos+Visa_mconsumospesos ]
# dataset[ , propia_mpagospesos :=  Master_mpagospesos+Visa_mpagospesos ]
# dataset[ , propia_ctarjeta :=  ctarjeta_master+ctarjeta_visa ]
# dataset[ , propia_cconsumos :=  Master_cconsumos+Visa_cconsumos ]
# dataset[ , propia_debitos_automaticos :=  mcuenta_debitos_automaticos+mttarjeta_visa_debitos_automaticos+mttarjeta_master_debitos_automaticos ]
dataset[ , propia_margen :=  mactivos_margen+mpasivos_margen ]
dataset[ , propia_mov :=  ctrx_quarter+active_quarter ]
dataset[ , propia_prestamos_personales :=  ifelse( cprestamos_personales==0, 0, mprestamos_personales/cprestamos_personales ) ]
dataset[ , propia_prestamos_prendarios :=  ifelse( cprestamos_prendarios==0, 0, mprestamos_personales/cprestamos_prendarios )]
dataset[ , propia_prestamos_hipotecarios :=  ifelse( cprestamos_hipotecarios==0, 0, mprestamos_personales/cprestamos_hipotecarios )]
dataset[ , propia_mrentabilidad :=  mrentabilidad/cliente_antiguedad ]
dataset[ , propia_mrentabilidad_annual :=  mrentabilidad_annual/cliente_antiguedad ]
dataset[ , propia_antiguedad :=  cliente_edad/cliente_antiguedad ]
dataset[ , propia_saldo_tarjetas :=  Master_msaldototal+Visa_msaldototal ]
dataset[ , propia_limite_tarjetas_edad :=  (Master_mlimitecompra+Visa_mlimitecompra)/cliente_edad ]
dataset[ , propia_limite_tarjetas_ant :=  (Master_mlimitecompra+Visa_mlimitecompra)/cliente_antiguedad ]
dataset[ , propia_rank_mcuentas_saldo :=frank(-mcuentas_saldo,ties.method = "dense")]
dataset[ , propia_rank_mcuenta_corriente :=frank(-mcuenta_corriente,ties.method = "dense")]
dataset[ , propia_rank_mcaja_ahorro :=frank(-mcaja_ahorro,ties.method = "dense")]
dataset[ , propia_rank_mcaja_ahorro_dolares :=frank(-mcaja_ahorro_dolares,ties.method = "dense")]

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

# 
# modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
#                  data=      dtrain,  #los datos donde voy a entrenar
#                  xval=         0,
#                  cp=          -0.65,#  -0.89
#                  minsplit=  606,   # 621
#                  minbucket=  205,   # 309
#                  maxdepth=     9 )  #  12

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.54,#  -0.89
                 minsplit=  1073,   # 621
                 minbucket=  278,   # 309
                 maxdepth=     9 )  #  12

#----------------------------------------------------------------------------
# habilitar esta seccion si el Fiscal General  Alejandro Bolaños  lo autoriza
#----------------------------------------------------------------------------

# corrijo manualmente el drifting de  Visa_fultimo_cierre
dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]


#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(490577)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )


dir.create( "./exp/" )
dir.create( "./exp/KA4120" )


#for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
for( corte  in  c( 7500, 7750, 9800, 8250, 8500 ) )
    
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]


  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
           file= paste0( "./exp/KA4120/KA4120_005_",  corte, ".csv"),
           sep=  "," )
}

vector_importantes <- names( modelo$variable.importance )
vector_importantes
