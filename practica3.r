####################################################################################################
###################################### Configuración inicial #######################################
# Paquetes necesarios
#install.packages("rpart")
#install.packages("nnet")
library("rpart")
library("nnet")
set.seed(96)

# Imporamos los datos a utilizar
zillow<-read.csv2("HousePricesData-Address-City-Features_fromZillow.csv")

# Nos da una muestra compacta de la estructura de zillow
#str(zillow)

# Creamos una variable que contenga las columnas que deseamos eliminar
cols.dont.want <- "street"
# Removemos la columna "Street" del data frame (por motivos de privacidad)
zillow <- zillow[,!names(zillow) %in% cols.dont.want, drop=F]

# ya que la variable "cols.dont.want" es de un solo uso, la eliminamos posterior a su uso
rm(cols.dont.want)

# Definimos la función para calcular la Raíz del error cuadrático medio RMSE
RMSE<-function(p,o){sqrt(mean((p-o)^2))}
# Definimos la función para calcular el error absoluto medio MAE
MAE<-function(p,o){mean(abs(p-o))}

####################################################################################################
###################################### Distribución de datos #######################################
# Mediante la función floor() determinamos cual es el valor entero bajo más cercano al 75% deseado
tam_muestra <- floor(0.75 * nrow(zillow))
# Se crea una lista de índices con el 75% de los datos
train_ind <- sample(seq_len(nrow(zillow)),size=tam_muestra)
# Con los índices listos, se crea un conjunto de entrenamiento con el 75% de los datos de zillow
train <- zillow[train_ind,]
# Con el 25% restante se crea un conjunto de Test para validación posterior al entrenamiento
test <- zillow[-train_ind,]

####################################################################################################
##################################### Entrenamiento de modelos #####################################

# Para entrenar un modelo remueva # de la línea correspondiente
# Se recomienda entrenar uno por vez. Aunque ya se tenga establecida una semilla de aleatoriedad, 
# entrenar un modelo por vez nos permite tener una mejor comparación entre modelos.

#model.lm<-lm(price~zipcode+year+bath+bed+rooms+SqFt, data=train)
#model.cart<- rpart(price~zipcode+year+bath+bed+rooms+SqFt,method="anova",data=train)
#model.nn<-nnet(price~zipcode+year+bath+bed+rooms+SqFt,data=train, size =12, skip=TRUE,linout=TRUE,decay=0.025)

####################################################################################################
###################################### Prueba de los modelos #######################################
# Para este ejemplo vamos a observar los valores de predicción de los datos de test y entrenamiento.
# Para ello, necesitamos los valores de precio de cada conjunto de datos, es por ello que se asignan
# estos valores a las variables correspondientes.
prices_train<-train[,10]
prices_test<-test[,10]

# Al tener los modelos entrenados, procedemos a almacenar el resultado 
# de las predicciones basadas en el conjunto train en las variables correspondientes
predictlmt<-predict(model.lm,train)
predictcartt<-predict(model.cart,train)
predictnnett<-predict(model.nn,train)
# Al tener los modelos entrenados, procedemos a almacenar el resultado 
# de las predicciones basadas en el conjunto test en las variables correspondientes
predictlm<-predict(model.lm,test)
predictcart<-predict(model.cart,test)
predictnnet<-predict(model.nn,tests)

# Para observar el MAE y el RMSE aplicados al conjunto predicho por el modelo 
# junto con los datos proporcionados, remueva el # de la línea correspondiente
#MAE(prices_train,predictlmt)
#RMSE(prices_train,predictlmt)
#MAE(prices_train,predictcartt)
#RMSE(prices_train,predictcartt)
#MAE(prices_train,predictnnett)
#RMSE(prices_train,predictnnett)
#MAE(prices_test,predictlm)
#RMSE(prices_test,predictlm)
#MAE(prices_test,predictcart)
#RMSE(prices_test,predictcart)
#MAE(prices_test,predictnnet)
#RMSE(prices_test,predictnnet)

####################################################################################################