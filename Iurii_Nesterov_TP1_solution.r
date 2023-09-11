rm(list = ls())
gc()

require("data.table")
require("rlist")
require("rpart")
require("parallel")
require("DiceKriging")
require("mlrMBO")


param <- list()
param$minsplit <- 1186
param$minbucket <- 208
param$maxdepth <- 9
param$corte <- 8668
param$cp <- -1
param$semilla_azar <- 181081


setwd("C:\\Users\\USER\\DMEyF")
dataset <- fread("competencia_01.csv")
dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]

dtrain <- dataset[foto_mes==202103]
dapply <- dataset[foto_mes==202105]

set.seed( param$semilla_azar )

modelo <- rpart("clase_binaria ~ . - clase_ternaria",
                data = dtrain,
                xval = 0,
                control = param
)

prediccion <- predict(modelo,
                      dapply,
                      type = "prob"
)
prob_baja <- prediccion[, "POS"]

tablita <- copy( dapply[, list(numero_de_cliente) ] )
tablita[ , prob := prob_baja ]
setorder( tablita, -prob )

tablita[ , Predicted := 0L ]
tablita[ 1:param$corte, Predicted := 1L ]

nom_submit <- paste0("BO_R_sub.csv" )
fwrite( tablita[ , list(numero_de_cliente, Predicted)],
        file= nom_submit,
        sep= "," )
