#               CombinarMediaDesviaciontipica.R
#
# https://github.com/FedericoTe/Estadistica
#
# Realizado por Federico Tejeiro 2020
#



# Parte 1: Introducción de Datos

# FALTA MANEJO DE ERRORES DE USUARIO

m = as.integer(readline("Introduce el número de observaciones: ") )

myData <- array(1:as.integer(m),c(as.integer(m),3))  # array con tres columnas: N_i, Mean_i, SD_i

for (i in 1:m)
{
  myData[i,1] <-as.numeric(readline (paste("Introduce la muestra (N_i) del estudio " , as.numeric(i) , ": ")))
  myData[i,2] <-as.numeric(readline (paste("Introduce la media (Mean_i) del estudio " , as.numeric(i) , ": ")))
  myData[i,3] <-as.numeric(readline (paste("Introduce la Desviación estándard (SD_i) del estudio " , as.numeric(i) , ": ")))
}
  
# Parte 2: Uso de la fórmula de Cochrane extraída en:
# https://handbook-5-1.cochrane.org/chapter_7/table_7_7_a_formulae_for_combining_groups.htm
# O también es:
# https://training.cochrane.org/handbook/current/chapter-06#_Ref536445431
#
# considerar que:  usual pooled standard deviation, which provides a slight underestimate of the desired standard deviation. Extraído de:
# http://handbook-5-1.cochrane.org/chapter_7/7_2_6_0_introductory_text.htm  Y del enlace anterior puesto.

# FALTA CRITERIOS DE APLICACIÓN DE LA FÓRMULA: Normalidad de las poblaciones, Homocedasticidad o igualdad de varianzas (ANOVA??)

Calculos <- function(Datos)  # funcion que calcula la N_total, Media_total, SD_total
  {
  N_t <- 0
  Mean_t <- 0
  SD_t <- 0
  for (i in 1:nrow(Datos))
    {
    N_1 = N_t
    M_1 = Mean_t
    SD_1 = SD_t
    N_2 = as.numeric(Datos[i,1])
    M_2 = as.numeric(Datos[i,2])
    SD_2 = as.numeric(Datos[i,3])
    
    N_t = N_1 + N_2
    Mean_t = (N_1*M_1 + N_2*M_2)/(N_1+N_2)
    SD_t = sqrt( ((N_1-1)*SD_1**2 + (N_2-1)*SD_2**2+(N_1*N_2*(M_1-M_2)**2/(N_1+N_2-1)))/(N_1+N_2-1))
  }
  Datos_devolver = c (N_t,Mean_t,SD_t)
  return(Datos_devolver)
}

# Parte 3: visualización de los datos globales

Datos_totales = Calculos(myData)
names(Datos_totales) <-c("  Muestra", "      Media", "     Desviación Típica")
print("Valores combinados")
print(Datos_totales)

# FIN
