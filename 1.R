#Laboratorio 1 - An�lisis de Datos
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

#Los nombres originales de las variables, una breve explicaci�n y los tipos de los datos:

#age (rango de edad): 10-19, 20-29, 30-39, 40-49,.
#menopause (momento de la menopausia): lt40, ge40, premeno.
#tumor-size (tama�o del tumor extirpado en mm): 0-4, 5-9, 10-14, .
#inv-nodes (una m�trica de presencia de c�lulas cancerosas en los nodos linf�ticos): 0-2, 3-5, 6-8, 9-11,.
#node-caps (evidencia de que c�lulas cancerosas atravesaron la c�psula de los n�dulos linf�ticos): yes, no
#deg-malig (grado histol�gico del tumor: bajo, intermedio, alto): 1, 2, 3.
#breast (mama afectada): left, right.
#breast-quad (cuadrante de la mama): left-up, left-low, right-up, right-low, central.
#irradiat (radioterapia): yes, no.
#Class (clase) Indica recurrencia, es la variable a predecir (no-recurrencia: 201 casos, recurrencia: 85 casos)


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
tabla$class <- as.factor(tabla$class)
tabla$age <- as.factor(tabla$age) 
tabla$menopause <- as.factor(tabla$menopause) 
tabla$tumor.size <- as.factor(tabla$tumor.size)
tabla$inv.nodes <- as.factor(tabla$inv.nodes) 
tabla$node.caps <- as.factor(tabla$node.caps) 
tabla$breast <- as.factor(tabla$breast) 
tabla$breast.quad <- as.factor(tabla$breast.quad) 
tabla$irradiat <- as.factor(tabla$irradiat)

#Lo primero es ver el rango de las variables, algunas medidas
#de tendencia central para las variables continuas, recuentos 
#para las categ�ricas y presencia de valores faltantes.
summary(tabla)

#Se sacan los datos nulos del datagrama
bool.values <- tabla$node.caps=='?'
tabla <- tabla[!bool.values,]

bool.values <- tabla$breast.quad =='?'
tabla <- tabla[!bool.values,]

summary(tabla)

#Hay algunas variables que de antemano podemos sospechar 
#que est�n relacionadas con otras. 
#Un ejemplo de esto es la relaci�n entre recurrencia 
#con las dem�s variables
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

#-------------------------------------------------------------------------------
class_ageGrafico <- as.data.frame(class_age)

#Gr�fico de frecuencias de edades dependiendo el tipo de clase
ggplot(data=class_ageGrafico, aes(x=age , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_menopauseGrafico <- as.data.frame(class_menopause)

#Gr�fico de frecuencias de tipo de menopausia dependiendo el tipo de clase
ggplot(data=class_menopauseGrafico, aes(x=menopause , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_tumor.sizeGrafico <- as.data.frame(class_tumor.size)

#Gr�fico de frecuencias de tama�o de tumor dependiendo el tipo de clase
ggplot(data=class_tumor.sizeGrafico, aes(x=tumor.size , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_inv.nodesGrafico <- as.data.frame(class_inv.nodes)

#Gr�fico de frecuencias de inv.nodes dependiendo el tipo de clase
ggplot(data=class_inv.nodesGrafico, aes(x=inv.nodes , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_node.capsGrafico <- as.data.frame(class_node.caps)

#Gr�fico de frecuencias de node.caps dependiendo el tipo de clase
ggplot(data=class_node.capsGrafico, aes(x=node.caps , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_deg.maligGrafico <- as.data.frame(class_deg.malig)

#Gr�fico de frecuencias de deg.malig dependiendo el tipo de clase
ggplot(data=class_deg.maligGrafico, aes(x=deg.malig , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_breastGrafico <- as.data.frame(class_breast)

#Gr�fico de frecuencias de breast dependiendo el tipo de clase
ggplot(data=class_breastGrafico, aes(x=breast , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_breast.quadGrafico <- as.data.frame(class_breast.quad)

#Gr�fico de frecuencias de breast.quad dependiendo el tipo de clase
ggplot(data=class_breast.quadGrafico, aes(x=breast.quad , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

class_irradiatGrafico <- as.data.frame(class_irradiat)

#Gr�fico de frecuencias de breast dependiendo el tipo de clase
ggplot(data=class_irradiatGrafico, aes(x=irradiat , y=Freq, fill = class)) + 
  geom_bar(stat="identity", position=position_dodge())+ theme_minimal()+ theme_classic()

#-------------------------------------------------------------------------------

#Se realizan diagramas de cajas para cada tabla, con tal de comprobar la
#dispersi�n y normalidad de las clases seg�n a la variable a comparar.
p1 <- ggboxplot(
  class_ageGrafico,
  x = "class", y = "Freq", title = "Class seg�n distribuci�n por Age",
  xlab = "Tipos de clases", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class"),
  label = "age",
  repel = TRUE
)
print(p1)
#Para el caso de class_age, se puede ver que la class no-recurrence-events posee
#una mejor distribuci�n y una mayor normalidad que la clase recurrence-events,
#sin considerar las cantidades totales para cada clase.

p2 <- ggboxplot(
  class_menopauseGrafico,
  x = "class", y = "Freq", title = "Class seg�n distribuci�n por Menopause",
  xlab = "Tipos de clases", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class"),
  label = "menopause",
  repel = TRUE
)
print(p2)
#Para el caso de class_menopause, se puede ver que la class recurrence-events
#posee una mejor distribuci�n y una mayor normalidad que la clase 
#no-recurrence-events, pues esta �ltima tiene un dato (lt40) esta muy alejada.

p3 <- ggboxplot(
  class_tumor.sizeGrafico,
  x = "class", y = "Freq", title = "Class seg�n distribuci�n por Tumor.size",
  xlab = "Tipos de clases", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class"),
  label = "tumor.size",
  repel = TRUE
)
print(p3)
#Para el caso de class_tumor.size, se puede ver que la class no-recurrence-events
#posee una mejor distribuci�n y una mayor normalidad que la clase 
#recurrence-events, pues esta �ltima tiene 2 datos alejados del centro.

p4 <- ggboxplot(
  class_inv.nodesGrafico,
  x = "class", y = "Freq", title = "Class seg�n distribuci�n por Inv.nodes",
  xlab = "Tipos de clases", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class"),
  label = "inv.nodes",
  repel = TRUE
)
print(p4)
#Para el caso de class_inv.nodes, ambas clases no poseen una normalidad dado que
#sus datos estan fuera del rango de sus respectivas cajas, sobre todo para la
#clase no-recurrence-events.

p5 <- ggboxplot(
  class_node.capsGrafico,
  x = "class", y = "Freq", title = "Class seg�n distribuci�n por Node.caps",
  xlab = "Tipos de clases", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class"),
  label = "node.caps",
  repel = TRUE
)
print(p5)
#Para el caso de class_node.caps, como ambas clases presentan 2 tipos de datos,
#tienen una normalidad perfecta. Se necesitarian m�s tipos de datos para que se
#presente una variaci�n de su normalidad. Solo var�an en sus cantidades totales.

p6 <- ggboxplot(
  class_deg.maligGrafico,
  x = "class", y = "Freq", title = "Class seg�n distribuci�n por Deg.malig",
  xlab = "Tipos de clases", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class"),
  label = "deg.malig",
  repel = TRUE
)
print(p6)
#Para el caso de class_deg.malig, la clase recurrence-events presenta una mejor
#distribuci�n y normalidad con respecto a la clase no-recurrence-events, dado
#que esta �ltima tiene un dato casi fuera de rango.

p7 <- ggboxplot(
  class_breastGrafico,
  x = "class", y = "Freq", title = "Class seg�n distribuci�n por Breast",
  xlab = "Tipos de clases", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class"),
  label = "breast",
  repel = TRUE
)
print(p7)
#Para el caso de class_breast, como ambas clases presentan 2 tipos de datos,
#tienen una normalidad perfecta. Se necesitarian m�s tipos de datos para que se
#presente una variaci�n de su normalidad. Solo varias en sus cantidades totales.

p8 <- ggboxplot(
  class_breast.quadGrafico,
  x = "class", y = "Freq", title = "Class seg�n distribuci�n por Breast.quad",
  xlab = "Tipos de clases", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class"),
  label = "breast.quad",
  repel = TRUE
)
print(p8)
#Para el caso de class_breast.quad, la clase recurrence-events presenta una mejor
#distribuci�n y normalidad con respecto a la clase no-recurrence-events, dado
#que esta �ltima tiene un promedio m�s alejado hacia abajo que m�s centralizado.

p9 <- ggboxplot(
  class_irradiatGrafico,
  x = "class", y = "Freq", title = "Class seg�n distribuci�n por Irradiat",
  xlab = "Tipos de clases", ylab = "Cantidad de individuos",
  color = "class",
  add = "jitter",
  add.params = list(color = "class", fill = "class"),
  label = "irradiat",
  repel = TRUE
)
print(p9)
#Para el caso de class_irradiat, como ambas clases presentan 2 tipos de datos,
#tienen una normalidad perfecta. Se necesitarian m�s tipos de datos para que se
#presente una variaci�n de su normalidad. Solo varias en sus cantidades totales.

#-------------------------------------------------------------------------------

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
tablaDatos = matrix(c(cor.class_age$cramer, cor.class_menopause$cramer,
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

#Gr�fico de las correlaciones de los distintos atributos con respecto a la clase

ggplot(data=tablaDeCorrelaciones, aes(x=reorder(Var2, -Freq), y=Freq, fill = Var2)) + 
  geom_bar(stat="identity", position="stack")+ theme_minimal()


#Se puede an�lizar este test para calcular la asociaci�n
with(as.data.frame(tabla), fisher.test( table(class, irradiat)))

#Esto indica que hay una asociaci�n significativa,
#pero antes de declarar esto, tenemos que considerar 
#que hay otras variables que pueden ser importante, y que 
#adem�s pueden presentar co-linealidad. Por lo tanto, 
#para estar seguros de esta asociaci�n, tenemos que controlar 
#las otras variables. Esto se puede probar con regresi�n 
#log�stica, que adem�s sirve como m�todo de clasificaci�n.

#C�lculamos las distintas asociaciones entre variables
#con respecto a la clase

with(as.data.frame(tabla), fisher.test(class_age))

with(as.data.frame(tabla), fisher.test(class_menopause))

with(as.data.frame(tabla), fisher.test(class_tumor.size, workspace = 2e8))

with(as.data.frame(tabla), fisher.test(class_inv.nodes))

with(as.data.frame(tabla), fisher.test(class_node.caps))

with(as.data.frame(tabla), fisher.test(class_deg.malig))

with(as.data.frame(tabla), fisher.test(class_breast))

with(as.data.frame(tabla), fisher.test(class_breast.quad))

with(as.data.frame(tabla), fisher.test(class_irradiat))

#Resumen
#Hasta el momento sabemos que hay variables
#que no parecen estar aociadas con la probabilidad de recurrencia:

#Edad
#Menopausia
#Mama
#Cuadrante
#Y otras variables que s� parecen ser importantes:

#Tama�o del tumor

#Cantidad de n�dulos comprometidos 
#Grado de malignidad (histol�gico)
#C�psula (celulas cancerosas fuera de las c�psulas de los n�dulos)
#Radioterapia

#Algunas de las variables no importantes, 
#pueden serlo cuando se las considera en conjunto con otras
#(interacciones), y algunas variables pueden presentar 
#colinealidad entre ellas; es decir, que est�n asociadas 
#entre ellas.
