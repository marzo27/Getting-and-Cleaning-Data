setwd("~/../Downloads/Coursera/3_Getting and Cleaning Data/")
library(data.table)
library(dplyr)

#Leer metadatos de apoyo
featureNames <- read.table("datos/UCI HAR Dataset/features.txt")
activityLabels <- read.table("datos/UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Leer datos de entrenamiento
subjectTrain <- read.table("datos/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("datos/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("datos/UCI HAR Dataset/train/X_train.txt", header = FALSE)

#Leer datos de prueba
subjectTest <- read.table("datos/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("datos/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("datos/UCI HAR Dataset/test/X_test.txt", header = FALSE)

#Tarea No 1. Combinar los conjuntos de entrenamiento y de prueba para crear un conjunto de datos
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#Tarea No. 2. Extraer la media y la desviacion estandard de las medidas
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
##[1] 10299   563

extractedData <- completeData[,requiredColumns]
dim(extractedData)
##[1] 10299    88


#Tarea No. 3. Usar nombres descriptivos para nombrar las actividades en los conjuntos de datos
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

#Tarea No. 4. Etiquetar apropiadamente los conjuntos de datos con nombres de variables descriptivos
names(extractedData)
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)

#Tarea No. 5. A partit del conjunto de datos en la tarea 4, cree un segundo conjunto de datos independiente y ordenado con el promedio de cada variable para cada actividad y cada sujeto
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "datos/UCI HAR Dataset/Tidy.txt", row.names = FALSE)






