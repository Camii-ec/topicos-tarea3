####################
#### Pregunta 2 ####
####################

library(tidyverse)


# Limpieza y preprocesamiento ---------------------------------------------

datos <- rio::import("loan.csv")

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
## Cuidao que la base de datos considera hasta el 2019, usamos la variable issue_d para filtrar sólo hasta 2015. Usamos esa variable, pues indica la fecha en que se entregó el préstamo

fecha <- str_split(datos$issue_d, "-", simplify = TRUE) %>% 
  data.frame() 

str(fecha)

fecha$X2 <- as.numeric(fecha$X2)

datos <- datos[which(fecha$X2 %in% 2007:2015),]

df <- datos %>% 
  select(purpose, int_rate, installment, annual_inc, dti, revol_bal, revol_util, inq_last_6mths, delinq_2yrs, pub_rec, loan_status)

saveRDS(df, file = "loan2.rds")

# Todo eso comentado, por si es que en algún momento lo necesitamos. Trabajar con loan2.rds, que es más pequeña y trabajable

df <- read_rds("loan2.rds")

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
# Hay un desbalance más o menos grande para las personas que no han pagado

# Vemos los NA
apply(df, 2, function(x) sum(is.na(x))) # En cantidad
apply(df, 2, function(x) sum(is.na(x)))/nrow(df) # en proporción
# La proporción de datos faltantes es muy pequeña. Se podría eliminar (por costo computacional, pofavo)

df <- na.omit(df)


# Separación de datos -----------------------------------------------------

library(tidymodels)
library(themis)

set.seed(3312)
id <- initial_split(data = df,
                    strata = loan_status)

train <- training(id)
test <- testing(id)

table(train$loan_status)
table(test$loan_status)

proc <- recipe(loan_status ~.,
              data = train) %>% 
  step_dummy(purpose) %>% 
  step_normalize(all_predictors()) %>% 
  step_tomek(loan_status)

cv <- vfold_cv(data = train, v = 3, strata = loan_status)


# Árbol de clasificación --------------------------------------------------

mod_arbol <- decision_tree(mode = "classification",
                           cost_complexity = tune(), # Costo de complejidad
                           tree_depth = tune()) %>%  # Profundidad
  set_engine("rpart", parms = list(split = "gini"))

w_arbol <- workflow() %>% 
  add_recipe(proc) %>% 
  add_model(mod_arbol)

hp_arbol <- extract_parameter_set_dials(mod_arbol) %>% 
  grid_regular(levels = list(cost_complexity = 5, tree_depth = 3))

grilla_arbol <- tune_grid(object = w_arbol,
                          resamples = cv,
                          metrics = metric_set(roc_auc),
                          control = control_grid(verbose = TRUE),
                          grid = hp_arbol)
beepr::beep(3) 

mod_arbolito_pro <- finalize_workflow(x = w_arbol,
                                      parameters = select_best(grilla_arbol, 
                                                               metric = "roc_auc")) %>% 
  fit(train)

saveRDS(mod_arbolito_pro, file = "modelos/arbol.rds")


# Random Forest -----------------------------------------------------------


