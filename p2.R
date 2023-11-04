####################
#### Pregunta 2 ####
####################

library(tidyverse)


# Limpieza y preprocesamiento ---------------------------------------------

#datos <- rio::import("loan.csv")

# purpuse: propósito del préstamo
# int_rate: tasa de interés en proporción
# installment: cuotas mensuales
# annual_inc: ingresos anuales auto-reportados
# dti: monto de deuda dividido por el ingreso anual
# revol_bal: monto no pagado al final del ciclo de facturación
# revol_util: monto de la línea de crédito utilizada en relación con el crédito total disponible
# inq_last_6mths: número de consultas crediticias del deudor realizadas por los prestatarios en los últimos 6 meses
# delinq_2yrs: cantidad de veces que ha tenido un pago vencido por más de 30 días en los últimos 2 años
# pub_rec: número de registros públicos derogados
# loan_status: estado del préstamo

#skimr::skim(datos)

# Más variables que la chucha las que no se usan. ¿Matar todo no más?
## Cuidao que la base de datos considera hasta el 2019, ¿consideramos todo eso o efectivamente eliminamos y sólo nos quedamos hasta 2015? ¿en volá usar el resto pa validación?

#df <- datos %>% 
  #select(purpose, int_rate, installment, annual_inc, dti, revol_bal, revol_util, inq_last_6mths, delinq_2yrs, pub_rec, loan_status)

#write_csv(df, file = "loan2.csv")

# Todo eso comentado, por si es que en algún momento lo necesitamos. Trabajar con loan2.csv, que es más pequeña y trabajable

df <- rio::import("loan2.csv")

glimpse(df)
skimr::skim(df)
# dti, revol_util, annual_inc, inq, delinq y pub son variables numéricas que contienen NA

table(df$loan_status)
## PREGUNTAR BIEN ESTO: ¿Sólo consideramos Fully Paid? es decir, el que no tiene tanto texto 

table(df$purpose)
## PREGUNTAR TAMBIÉN: esto tiene más categorías de las que se menciona en el enunciado, ¿debemos transformar para que sea como lo del enunciado? ¿o las trabajamos como vienen?

df$loan_status <- ifelse(df$loan_status == "Fully Paid", 1, 0) %>% 
  as.factor()

table(df$loan_status)/nrow(df)
# No parece ser un desbalance muy grande

# Vemos los NA
apply(df, 2, function(x) sum(is.na(x))) # En cantidad
apply(df, 2, function(x) sum(is.na(x)))/nrow(df) # en proporción
# La proporción de datos faltantes es muy pequeña. Se podría eliminar (por costo computacional, pofavo)

df <- na.omit(df)

