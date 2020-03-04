# -------------------------------------------------------------------------
# TALLER DE VISUALIZACIÓN DE DATOS CON GGPLOT2 ----------------------------
# -------------------------------------------------------------------------

#install.packages("nombredelpaquete")

# Lectura de datos --------------------------------------------------------

library(readr) 

Datos = read.csv2("https://raw.githubusercontent.com/RLadiesMedellin/Meetup2-Tallerggplot2/master/Data/EncuestaJuventud.csv", 
              header = TRUE, sep = ";", dec = ",") #Lectura de un csv

#Datos = read.delim2("https://raw.githubusercontent.com/RLadiesMedellin/Meetup2-Tallerggplot2/master/Data/EncuestaJuventud.txt", 
#                 header = TRUE, sep = "\t", dec = ",") #Lectura de un txt

head(Datos[,1:8], n = 5)


# Capa base ---------------------------------------------------------------

library(ggplot2)

ggplot(data = Datos, aes(x = Edad, y = GastoSemanalRumba)) #Capa base


# Capa geom ---------------------------------------------------------------

ggplot(data = Datos, aes(x = Edad, y = GastoSemanalRumba)) +
  geom_point() #Capa de puntos -- Forma 1

ggplot(data = Datos) +
  geom_point(aes(x = Edad, y = GastoSemanalRumba)) #Capa de puntos -- Forma 2


# Capa facet --------------------------------------------------------------

ggplot(data = Datos, aes(x = Edad, y = GastoSemanalRumba)) +
  geom_point() + 
  facet_wrap(~ Sexo)


# Capa Stats --------------------------------------------------------------

ggplot(data = Datos, aes(x = Edad, y = GastoSemanalRumba)) +
  geom_point() + 
  facet_wrap(~ Sexo) +
  stat_smooth(method = "lm", se = F, col = "red")



# Capa de coordenadas -----------------------------------------------------

ggplot(data = Datos, aes(x = Edad, y = GastoSemanalRumba)) +
  geom_point() + 
  facet_wrap(~ Sexo) + 
  stat_smooth(method = "lm", se = F, col = "red") + 
  scale_y_continuous("Gasto semanal en Rumba", breaks = seq(0,125000,10000))



# Capa de tema del gráfico ------------------------------------------------

ggplot(data = Datos, aes(x = Edad, y = GastoSemanalRumba)) + 
  geom_point() +
  facet_wrap(~ Sexo) + 
  stat_smooth(method = "lm", se = F, col = "red") + 
  scale_y_continuous("Gasto semanal en Rumba", breaks = seq(0,125000,10000)) +
  theme_bw()


# Interacción con más variables (I) ---------------------------------------

ggplot(data = Datos, aes(x = Edad, y = GastoSemanalRumba, color = Sexo)) +
  geom_point()


# Añadir interactividad a un gráfico de ggplot2 ---------------------------

library(plotly)

Plot = ggplot(data = Datos, aes(x = Edad, y = GastoSemanalRumba, color = Sexo)) +
  geom_point()

ggplotly(Plot) #Convierte un objeto ggplot2 a uno de plotly


# Interacción con más variables (II) --------------------------------------

ggplot(data = Datos) +
  geom_point(aes(x = Edad, y = GastoSemanalRumba, size = GastoSemanalTransporte),
             alpha = 0.5)


# Otros gráficos ----------------------------------------------------------

ggplot(data = Datos, aes(x = Sexo, y = GastoSemanalRumba)) +
  geom_boxplot()

ggplot(data = Datos, aes(x = Sexo, y = GastoSemanalRumba, fill = Sexo)) +
  geom_boxplot() + 
  theme_bw() + #Tema de fondo del gráfico
  theme(legend.position="top", plot.title = element_text(face = "bold", 
                                                         size =12,
                                                         hjust = 0.5), 
        plot.subtitle = element_text(size = 10, hjust = 0.5)) + 
  labs(y = "Gastos semanales en pesos",
       title = "Gastos semanales en rumba vs sexo", subtitle = "Taller ggplot2",
       caption = "Encuesta Juventud | MEDATA")


ggplot(data = Datos, aes(x = Sexo, y = GastoSemanalRumba, fill = Sexo)) + 
  geom_boxplot()+ 
  theme_bw() + 
  theme(legend.position="top", plot.title = element_text(face = "bold", size = 12,hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        axis.text.x = element_text(size = 12, angle = 45, colour = "blue", vjust = 0.5)) + 
  labs(y = "Gastos semanales en pesos", title = "Gastos semanales en rumba vs sexo",
       subtitle = "Taller ggplot2", caption = "Encuesta Juventud | MEDATA")


# Gráfico de barras -------------------------------------------------------

library(scales)
library(dplyr)


Datos %>% 
  group_by(ValoracionOportunidadesDeLaCiudadParaEstudiar) %>% 
  summarise(Frecuencia = n()) %>% 
  
  ggplot(aes(x= ValoracionOportunidadesDeLaCiudadParaEstudiar, y= Frecuencia)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#702899") +
  theme_bw() + 
  labs(x = "Oportunidades para estudiar")


Datos %>% 
  group_by(ValoracionOportunidadesDeLaCiudadParaEstudiar) %>% 
  summarise(Frecuencia = n()) %>%
  
  ggplot(aes(x= ValoracionOportunidadesDeLaCiudadParaEstudiar, y= Frecuencia)) +
  geom_bar(stat = "identity", width = 0.5, fill = "#702899") + 
  theme_bw() +
  labs(x = "Oportunidades para estudiar") + 
  coord_flip() # Permite rotar las barras en posición horizontal


# Ordenar categorías del eje X y agregar etiquetas de datos  ---------------

Datos %>% 
  group_by(ValoracionOportunidadesDeLaCiudadParaEstudiar) %>% 
  summarise(Frecuencia = n()) %>% 
  
  ggplot(aes(x= ValoracionOportunidadesDeLaCiudadParaEstudiar, y= Frecuencia)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#702899") +
  theme_bw() +
  labs(x = "Oportunidades para estudiar")+
  geom_text(size = 3.5,aes(label =  Frecuencia), vjust = -0.5)+ 
  scale_y_continuous(breaks = seq(0,4000,500)) +
  scale_x_discrete(limits = c("Muy desfavorable", "Desfavorable",
                              "Ni favorable/ Ni desfavorable",
                              "Favorable", "Muy favorable")) 


# Uso de la función facet_wrap --------------------------------------------

Datos %>% 
  group_by(ValoracionOportunidadesDeLaCiudadParaEstudiar, Sexo) %>%
  summarise(Frecuencia = n()) %>% 
  
  ggplot(aes(x= ValoracionOportunidadesDeLaCiudadParaEstudiar, y= Frecuencia)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#702899") +
  theme_bw() +
  labs(x = "Oportunidades para estudiar")+
  geom_text(size = 3.5,aes(label =  Frecuencia), vjust = -0.5)+ 
  scale_y_continuous(breaks = seq(0,4000,500)) +
  scale_x_discrete(limits = c("Muy desfavorable", "Desfavorable",
                              "Ni favorable/ Ni desfavorable",
                              "Favorable", "Muy favorable")) +
  facet_wrap(~ Sexo) #También podemos jugar con el argumento scales


# Mejorando el aspecto de las etiquetas del eje X -------------------------

Datos %>% group_by(ValoracionOportunidadesDeLaCiudadParaEstudiar, Sexo) %>%
  summarise(Frecuencia = n()) %>% 
  
  ggplot(aes(x= ValoracionOportunidadesDeLaCiudadParaEstudiar, y= Frecuencia)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#702899") +
  theme_bw() +
  labs(x = "Oportunidades para estudiar")+
  geom_text(size = 3.5,aes(label =  paste(Frecuencia)), vjust = -0.5)+ 
  scale_y_continuous(breaks = seq(0,4000,500)) +
  scale_x_discrete(limits = c("Muy desfavorable", "Desfavorable",
                              "Ni favorable/ Ni desfavorable",
                              "Favorable", "Muy favorable"),
                   labels = c("Muy \ndesfavorable", "Desfavorable",
                              "Ni favorable \nNi desfavorable",
                              "Favorable", "Muy \nfavorable")) +
  facet_wrap(~ Sexo) 

# Histograma --------------------------------------------------------------

ggplot(Datos, aes(x=Edad))+
  geom_histogram(breaks = seq(14,26,1), fill="#702899", color="black", bins = 10) +
  theme_bw() + 
  labs(y = "Frecuencia") + 
  scale_x_continuous(breaks = seq(14,26,2))


# Agregar líneas verticales y texto al gráfico ----------------------------

ggplot(Datos, aes(x=Edad))+
  geom_histogram(breaks = seq(14,26,1), fill="#702899", color="black", bins = 10) +
  theme_bw() + 
  labs(y = "Frecuencia") + scale_x_continuous(breaks = seq(14,26,2)) +
  geom_vline(xintercept = 18,linetype="dashed", size=1.5, colour="#b82b14") +
  annotate("text", label = "Mayores de edad", x = 22, y = 1600, size = 6, 
           colour = "#b82b14") 


# Capa de densidad en un histograma ---------------------------------------

ggplot(Datos, aes(x=Edad))+
  geom_histogram(breaks = seq(14,26,1), aes(y =..density..),
                 fill="#702899", color="black", bins = 10) +
  geom_density(fill="gray", alpha = 0.5, lwd = 0.7) +
  theme_bw() + 
  labs(y = "Densidad") + 
  scale_x_continuous(breaks = seq(14,26,2)) 


# Paletas de colores ------------------------------------------------------

library(RColorBrewer) #Paleta de colores

ggplot(Datos, aes(x=Edad, colour = Sexo, fill = Sexo))+
  geom_histogram(breaks = seq(14,26,1), bins = 10, position = "dodge", alpha = 0.7)+
  theme_bw() + 
  labs(y = "Frecuencia") + 
  scale_x_continuous(breaks = seq(14,26,2)) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2")
