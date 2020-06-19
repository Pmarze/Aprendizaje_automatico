####################################################################################################
###################################### Configuraci�n inicial #######################################

# Se utiliza la libreria "caret"
library(caret)  
# Para poder reproducir el entrenamiento definimos una semilla
# Para utilizar una de las dos semillas disponibles solo debe borrar el "#" correspondiente 
# y a�adir "#" a la semilla restante 

#Semilla utilizada para obtener los primeros datos
set.seed(7)     

# Semilla utilizada para observar la dependencia de los resultados y la semilla seleccionada.
#set.seed(527)   

# Se importa el archivo con los datos y se guardan en ticData
ticData <- read.table("tic-tac-toe.dat.txt", header = FALSE, sep = ',') 

# Se asignan los nombres correspondientes a cada columna del data frame
colnames(ticData)<-c("topl","topm","topr","midl","midm","midr","botl","botm","botr","class")

# Nos aseguramos que la columna de datos a predecir sea un factor
ticData$class <- as.factor(ticData$class)

####################################################################################################
########################### Comprobar la existencia de valores faltantes ###########################

# Se define el vector "datosf", en este, se almacena si hay o no, datos faltantes en el archivo importado
# complete.cases() permite saber si hay alg�n dato faltante (na) en ticData
# Unique retorna un vector sin los duplicados de complete.cases()

datosf <- unique(complete.cases(ticData))
# print(datosf)  #Para observar el comportamiento de datosf

# De datosf, tenemos 3 casos posibles... (TRUE),(TRUE,FALSE),(FALSE)

# Si datosf tiene (TRUE,FALSE), sabemos que falta como m�nimo un dato
if(length(datosf)>=2){
  print("Faltan datos")

# Si datosf tiene un solo elemento, este puede ser TRUE o FALSE
# Si el elemento es FALSE, el archivo no tiene datos
} else if(datosf[1]==F){
  print("Data Frame Vac�o")

# El caso donde no hacen falta datos es el �nico que nos interesa,
# por lo que, solo en este caso permitimos que el proceso continue.
} else {

####################################################################################################
###################################### Distribuci�n de datos #######################################

# Se crea una lista de �ndices con el 70% (p=0.7) de los datos de class
indices <- createDataPartition(ticData$class, p=0.7, list=F, times=1)
# Con los �ndices listos, se crea un conjunto de entrenamiento con el 70% de los datos de ticData
Train <-ticData[indices,]
# Con el 30% restante se crea un conjunto de Test para validaci�n posterior al entrenamiento
Test <- ticData[-indices,]

# El m�todo a utilizar es validaci�n cruzada con diez pliegues
fitControl <- trainControl(method = "cv", number = 10)

####################################################################################################
##################################### Entrenamiento de modelos #####################################

# Para entrenar un modelo remueva # de la l�nea correspondiente
# Se recomienda entrenar uno por vez. Aunque ya se tenga establecida una semilla de aleatoriedad, 
# entrenar un modelo por vez nos permite tener una mejor comparaci�n entre modelos.

#Fitnb <- train(class ~ . , data = Train, method = "nb", trcontrol = fitControl, verbose=F)
#FitC5.0 <- train(class ~ . , data = Train, method = "C5.0", trcontrol = fitControl, verbose=F)
#Fitnnet <- train(class ~ ., data = Train, method = "nnet", trcontrol = fitControl, verbose=F, trace=F)
#FitKKNN <- train(class ~ ., data=Train, method="kknn", trcontrol=fitControl, verbose=F)
#FitsvmL <- train(class ~ ., data = Train, method = "svmLinear", trcontrol = fitControl, verbose=F)

####################################################################################################
###################################### Prueba de los modelos #######################################

# Con el modelo ya entrenado, se procede a realizar predicciones con los datos de prueba.
# Al pasar estos dos par�metros en la funci�n predict(), obtenemos como resultado una lista de 
# predicciones, esta lista es almacenada por cada modelo en una variable que le corresponde.
nbPred <- predict(Fitnb,Test)
C5Pred <- predict(FitC5.0,Test)
nnetPred <- predict(Fitnnet,Test)
KKNNPred <- predict(FitKKNN,Test)
svmlPred <- predict(FitsvmL,Test)

# Para observar el rendimiento de cada modelo y comparar las predicciones con los valores reales
# se genera una matriz de confusi�n para cada modelo.
nbMatriz <- confusionMatrix(nbPred,Test$class)
C5Matriz <- confusionMatrix(C5Pred,Test$class)
nnetMatriz <- confusionMatrix(nnetPred,Test$class)
KNNMatriz <- confusionMatrix(KKNNPred,Test$class)
svmlMatriz <- confusionMatrix(svmlPred,Test$class)


####################################################################################################
# Fin del caso 3, donde datosf tiene todos los datos disponibles.
}
####################################################################################################