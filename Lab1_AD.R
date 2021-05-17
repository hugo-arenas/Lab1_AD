filename = "C:/Users/dratz/Desktop/Lab_Analisis-de-Datos/Lab1_AD/breast-cancer.data"
columns <- c("class", "age", "menopause", "tumor.size", "inv.nodes", 
             "node.caps","deg.malig", "breast", "breast.quad", "irradiat")
table <- read.csv(filename, col.names = columns) 
data.mean <- tapply(table$deg.malig, INDEX = table$class, FUN = mean)