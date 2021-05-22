#Laboratorio 1 - Anï¿½lisis de Datos
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
cor.class_deg.malig <- assocstats(class_deg.malig)
cor.class_breast <- assocstats(class_breast)
cor.class_breast.quad <- assocstats(class_breast.quad)
cor.class_irradiat <- assocstats(class_irradiat)


#Se crea una tabla para mostrar todo
tablaDatos = matrix(c(cor.class_age$cramer, cor.class_menopause$creamer,
                      cor.class_tumor.size$cramer, cor.class_inv.nodes$cramer,
                      cor.class_node.caps$cramer, cor.class_deg.malig$cramer,
                      cor.class_breast$cramer, cor.class_breast.quad$cramer,
                      cor.class_irradiat$cramer), ncol=9,byrow=TRUE)

#Se le asigna nombre a las columnas
colnames(tablaDatos) =  c("Age",
                     "Menopause",
                     "Tumor.size",
                     "Inv.nodes",
                     "Node.caps",
                     "Deg.malig",
                     "Breast",
                     "Breast.quad",
                     "Irradiat")
#y a las filas
rownames(tablaDatos) = c("Class"
)


#Finalmente se convierte a tabla y se muestra
tablaDeCorrelaciones=as.table(tablaDatos)
tablaDeCorrelaciones

tablaDeCorrelaciones <- as.data.frame(tablaDeCorrelaciones)

library (ggplot2) 

#Gráfico de las correlaciones de los distintos atributos con respecto a la clase

ggplot(data=tablaDeCorrelaciones, aes(x=reorder(Var2, -Freq), y=Freq, fill = Var2)) + 
    geom_bar(stat="identity", position="stack")+ theme_minimal()


class_ageGrafico <- as.data.frame(class_age)

#Gráfico de frecuencias de edades dependiendo el tipo de clase
ggplot(data=class_ageGrafico, aes(x=age , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_menopauseGrafico <- as.data.frame(class_menopause)

#Gráfico de frecuencias de tipo de menopausia dependiendo el tipo de clase
ggplot(data=class_menopauseGrafico, aes(x=menopause , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_tumor.sizeGrafico <- as.data.frame(class_tumor.size)

#Gráfico de frecuencias de tamaño de tumor dependiendo el tipo de clase
ggplot(data=class_tumor.sizeGrafico, aes(x=tumor.size , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_inv.nodesGrafico <- as.data.frame(class_inv.nodes)

#Gráfico de frecuencias de inv.nodes dependiendo el tipo de clase
ggplot(data=class_inv.nodesGrafico, aes(x=inv.nodes , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_node.capsGrafico <- as.data.frame(class_node.caps)

#Gráfico de frecuencias de node.caps dependiendo el tipo de clase
ggplot(data=class_node.capsGrafico, aes(x=node.caps , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_deg.maligGrafico <- as.data.frame(class_deg.malig)

#Gráfico de frecuencias de deg.malig dependiendo el tipo de clase
ggplot(data=class_deg.maligGrafico, aes(x=deg.malig , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_breastGrafico <- as.data.frame(class_breast)

#Gráfico de frecuencias de breast dependiendo el tipo de clase
ggplot(data=class_breastGrafico, aes(x=breast , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_breast.quadGrafico <- as.data.frame(class_breast.quad)

#Gráfico de frecuencias de breast.quad dependiendo el tipo de clase
ggplot(data=class_breast.quadGrafico, aes(x=breast.quad , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_irradiatGrafico <- as.data.frame(class_irradiat)

#Gráfico de frecuencias de breast dependiendo el tipo de clase
ggplot(data=class_irradiatGrafico, aes(x=irradiat , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

#Se realiza anova para las tablas.
dl.class_age <- data.frame(class_age)
p1 <- ggboxplot(
  dl.class_age,
  x = "class", y = "Freq",
  xlab = "Rango de años", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class"),
)
print(p1)
aov.class_age <- ezANOVA(
  data = dl.class_age, 
  dv = Freq,
  wid = age,
  within = class,
  type = 3,
  return_aov = TRUE
)

dl.class_menopause <- data.frame(class_menopause)
p2 <- ggboxplot(
  dl.class_menopause,
  x = "class", y = "Freq",
  xlab = "Rango de años", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class")
)
print(p2)
aov.class_menopause <- ezANOVA(
  data = dl.class_menopause, 
  dv = Freq,
  wid = menopause,
  within = class,
  type = 3,
  return_aov = TRUE
)

dl.class_tumor.size <- data.frame(class_tumor.size)
p3 <- ggboxplot(
  dl.class_tumor.size,
  x = "class", y = "Freq",
  xlab = "Rango de años", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class")
)
print(p3)
aov.class_tumor.size <- ezANOVA(
  data = dl.class_tumor.size, 
  dv = Freq,
  wid = tumor.size,
  within = class,
  type = 3,
  return_aov = TRUE
)

dl.class_inv.nodes <- data.frame(class_inv.nodes)
p4 <- ggboxplot(
  dl.class_inv.nodes,
  x = "class", y = "Freq",
  xlab = "Rango de años", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class")
)
print(p4)
aov.class_inv.nodes <- ezANOVA(
  data = dl.class_inv.nodes, 
  dv = Freq,
  wid = inv.nodes,
  within = class,
  type = 3,
  return_aov = TRUE
)

dl.class_node.caps <- data.frame(class_node.caps)
p5 <- ggboxplot(
  dl.class_node.caps,
  x = "class", y = "Freq",
  xlab = "Rango de años", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class")
)
print(p5)
aov.class_node.caps <- ezANOVA(
  data = dl.class_node.caps, 
  dv = Freq,
  wid = node.caps,
  within = class,
  type = 3,
  return_aov = TRUE
)

dl.class_deg.malig <- data.frame(class_deg.malig)
p6 <- ggboxplot(
  dl.class_deg.malig,
  x = "class", y = "Freq",
  xlab = "Rango de años", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class")
)
print(p6)
aov.class_deg.malig <- ezANOVA(
  data = dl.class_deg.malig, 
  dv = Freq,
  wid = deg.malig,
  within = class,
  type = 3,
  return_aov = TRUE
)

dl.class_breast <- data.frame(class_breast)
p7 <- ggboxplot(
  dl.class_breast,
  x = "class", y = "Freq",
  xlab = "Rango de años", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class")
)
print(p7)
aov.class_breast <- ezANOVA(
  data = dl.class_breast, 
  dv = Freq,
  wid = breast,
  within = class,
  type = 3,
  return_aov = TRUE
)

dl.class_breast.quad <- data.frame(class_breast.quad)
p8 <- ggboxplot(
  dl.class_breast.quad,
  x = "class", y = "Freq",
  xlab = "Rango de años", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class")
)
print(p8)
aov.class_breast.quad <- ezANOVA(
  data = dl.class_breast.quad, 
  dv = Freq,
  wid = breast.quad,
  within = class,
  type = 3,
  return_aov = TRUE
)

dl.class_irradiat <- data.frame(class_irradiat)
p9 <- ggboxplot(
  dl.class_irradiat,
  x = "class", y = "Freq",
  xlab = "Rango de años", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class")
)
print(p9)
aov.class_irradiat <- ezANOVA(
  data = dl.class_irradiat, 
  dv = Freq,
  wid = irradiat,
  within = class,
  type = 3,
  return_aov = TRUE
)

#Se puede análizar este test para calcular la asociación
test.fisher <- with(as.data.frame(tabla), fisher.test( table(class, irradiat)))

#Esto indica que hay una asociación significativa,
#pero antes de declarar esto, tenemos que considerar 
#que hay otras variables que pueden ser importante, y que 
#además pueden presentar co-linealidad. Por lo tanto, 
#para estar seguros de esta asociación, tenemos que controlar 
#las otras variables. Esto se puede probar con regresión 
#logística, que además sirve como método de clasificación.

