Seminario
# Análisis-de-correspondencia-múltiple(ACM)

#Paquetes necesarios

    library("FactoMineR")             
    library("factoextra")
    library(ggplot2)

#Cargar base de datos

    BD = read.csv2("D:/Especialización Estadistica/Semestre II/Seminario de Investigación/Tesis/BD/Bd filtradas/No_Mamografia.csv",         header = TRUE)
    str(BD)

#Abreviar nombre de los niveles de las variable

    levels(BD$Q951)
    abbreviate(levels(BD$Q951))
    BD$Q951 <- factor(abbreviate(BD$Q951))
    head(BD$Q951)

#Definir marco de datos de interes para ACM

    no.mamografia <- data.frame(BD[,c(29,7,3)])
    head(no.mamografia)

#Función para el ACM

    res.mca <- MCA(no.mamografia, graph = FALSE)
    print(res.mca)

#Imprime el resumen de los resultados del ACM

    summary(res.mca, nb.dec = 2, ncp = 3)

#Proporciona las varianzas retenidas por las diferentes dimensiones (ejes)

    eigenvalues <- get_eigenvalue(res.mca)
    head(round(eigenvalues, 2))

#Gráfica los porcentajes de inercia explicada por las dimensiones de MCA

    fviz_screeplot(res.mca)

#Marco de datos con las coordenadas de las variables

    datos = apply(no.mamografia, 2, function(x) nlevels(as.factor(x)))
    mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(datos), datos))

#Marco de datos con las coordenadas observadas

    mca1_obs_df = data.frame(mca1$ind$coord)

##Grafica las coordenadas de filas y columnas presentadas en la salida del ACM

    ggplot(data=mca1_vars_df,
         aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
      theme_minimal()+
      geom_hline(yintercept = 0, colour = "black",lty=2) +
      geom_vline(xintercept = 0, colour = "black",lty=2) +
      geom_text(aes(colour=Variable)) +
      scale_colour_discrete(name ="Variables")+
      ggtitle("")
