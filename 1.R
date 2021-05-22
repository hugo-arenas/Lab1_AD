#Laboratorio 1 - Análisis de Datos
#Integrantes:
#            -Hugo Arenas
#            -Juan Arredondo

library(ez)
library(dplyr)
library(ggpubr)
library(knitr)
library(tidyr)
library(car)
library(lmtest)
library(vcd)

#1. Class: no-recurrence-events, recurrence-events
#2. age: 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80-89, 90-99.
#3. menopause: lt40, ge40, premeno.
#4. tumor-size: 0-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59.
#5. inv-nodes: 0-2, 3-5, 6-8, 9-11, 12-14, 15-17, 18-20, 21-23, 24-26, 27-29, 30-32, 33-35, 36-39.
#6. node-caps: yes, no.
#7. deg-malig: 1, 2, 3.
#8. breast: left, right.
#9. breast-quad: left-up, left-low, right-up, right-low, central.
#10. irradiat: yes, no.

# Leemos los datos
dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)
filename <- "breast-cancer.data"
file <- file.path(dirstudio, filename)

#Se definen los nombres de las columnas, estos son los mismos de provistos por la base de datos
columns <- c("class", 
             "age", 
             "menopause", 
             "tumor.size", 
             "inv.nodes", 
             "node.caps",
             "deg.malig",
             "breast",
             "breast.quad",
             "irradiat")

tabla <- read.csv(file, col.names = columns)
tabla <- tabla 
tabla$class <- as.factor(tabla$class)
tabla$age <- as.factor(tabla$age) 
tabla$menopause <- as.factor(tabla$menopause) 
tabla$tumor.size <- as.factor(tabla$tumor.size)
tabla$inv.nodes <- as.factor(tabla$inv.nodes) 
tabla$node.caps <- as.factor(tabla$node.caps) 
tabla$breast <- as.factor(tabla$breast) 
tabla$breast.quad <- as.factor(tabla$breast.quad) 
tabla$irradiat <- as.factor(tabla$irradiat)


summary(tabla)

#Se sacan los datos nulos del datagrama
bool.values <- tabla$node.caps=='?'
tabla <- tabla[!bool.values,]

bool.values <- tabla$breast.quad =='?'
tabla <- tabla[!bool.values,]

summary(tabla)

attach(tabla)
#age_class<- table(age, class)
#age_class

#age_menopause<- table(age, menopause)
#age_menopause

#age_tumor.size<- table(age, tumor.size)
#age_tumor.size

#age_inv.nodes<- table(age, inv.nodes)
#age_inv.nodes

#age_node.caps<- table(age, node.caps)
#age_node.caps
#age_node.caps[, -1]

#age_deg.malig<- table(age, deg.malig)
#age_deg.malig

#age_breast<- table(age, breast )
#age_breast

#age_breast.quad<- table(age, breast.quad)
#age_breast.quad
#age_breast.quad[, -1]

class_age <- table(class, age)
class_age

class_menopause <- table(class, menopause)
class_menopause

class_tumor.size <- table(class, tumor.size)
class_tumor.size

class_inv.nodes <- table(class, inv.nodes)
class_inv.nodes

class_node.caps <- table(class, node.caps)
class_node.caps <- class_node.caps[, -1]
class_node.caps

class_deg.malig <- table(class, deg.malig)
class_deg.malig

class_breast <- table(class, breast)
class_breast

class_breast.quad <- table(class, breast.quad)
class_breast.quad <- class_breast.quad[, -1]
class_breast.quad

class_irradiat <- table(class, irradiat)
class_irradiat

#Calculamos las distintas correlaciones entre los datos.

#En este caso las correlaciones entre la clase principal y los atributos

cor.class_age <- assocstats(class_age)
cor.class_menopause <- assocstats(class_menopause)
cor.class_tumor.size <- assocstats(class_tumor.size)
cor.class_inv.nodes <- assocstats(class_inv.nodes)
cor.class_node.caps <- assocstats(class_node.caps)
cor.class_breast <- assocstats(class_breast)
cor.class_breast.quad <- assocstats(class_breast.quad)
cor.class_irradiat <- assocstats(class_irradiat)


#Se crea una tabla para mostrar todo
tablaDatos = matrix(c(cor.class_age$cramer, cor.class_menopause$creamer,
                      cor.class_tumor.size$cramer, cor.class_inv.nodes$cramer,
                      cor.class_node.caps$cramer, cor.class_breast$cramer,
                      cor.class_breast.quad$cramer, cor.class_irradiat$cramer),ncol=8,byrow=TRUE)

#Se le asigna nombre a las columnas
colnames(tablaDatos) =  c("Age",
                     "Menopause",
                     "Tumor.size",
                     "Inv.nodes",
                     "Node.caps",
                     "Breast",
                     "Breast.quad",
                     "Irradiat"
)
#y a las filas
rownames(tablaDatos) = c("Class"
)


#Finalmente se convierte a tabla y se muestra
tablaDeCorrelaciones=as.table(tablaDatos)
tablaDeCorrelaciones

dl.class_age <- xtabs(~ class + age, data = tabla2)
dl.class_age <- data.frame(dl.class_age)
ez.aov <- ezANOVA(
  data = dl.class_age, 
  dv = Freq,
  wid = class,
  within = age,
  type = 3,
  return_aov = TRUE
)

