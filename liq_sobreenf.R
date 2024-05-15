library(readr)
library(ggplot2)

# Elige el rayo con el intercepto más cercano a 0

distime<-function(data,tiempo,r){

  cnonul<-list()
  for( i in 2:length(data)){
    if(sum(data[[i]][1:tiempo])!=0){
      cnonul[length(cnonul)+1]<-i
    }
  }
  
  time<-data[1:tiempo,1]
  data<-data[1:tiempo,unlist(cnonul)]
  
  qls<-t(apply(data, 1, quantile,probs = c(0.25, 0.5, 0.75)))
  iqr<-qls[,3]-qls[,1]
  lim_inf<-qls[,1]-1.5*iqr
  lim_sup<-qls[,3]+1.5*iqr
  
  dt_mediana<-list()
  use_ray<-data<lim_sup & data>lim_inf
  for(j in 1:nrow(data)){
    dt_mediana[length(dt_mediana)+1]<-median(as.numeric(data[j,use_ray[j,]]))
  }

  ndata<-data.frame(time,unlist(dt_mediana)) 
  names(ndata)<-c("Tiempo","Promedio")
  
  model<-lm(ndata$Promedio ~ ndata$Tiempo)
  names(model$coefficients)<-c("Intercepto","Velocidad")
  
  predicciones <- predict(model, interval = "confidence")
  d_pred <- data.frame(Tiempo = ndata$Tiempo, Distancia =ndata$Promedio , pred = predicciones[, 1],
                       lower = predicciones[, 2], upper = predicciones[, 3])
  
  graf<-ggplot(d_pred, aes(x = Tiempo, y = Distancia)) +
    geom_point(aes(y = Distancia),color = "#2297E6",size=3,shape = 4) +  
    geom_line(aes(y = Distancia),color = "#2297E6",size=1) +                           # Puntos de datos
    geom_line(aes(y = pred), color = "red",size=1) +              # Línea de regresión
    # geom_ribbon(aes(ymin = lower, ymax = upper),           # Intervalo de confianza
    #             fill = "lightblue", alpha = 0.5) +
    theme(legend.position = "bottom",  # Posición de la leyenda
          plot.title = element_text(hjust = 0.5,size = 16),  # Centra el título
          panel.grid.major = element_line(color = "gray"),
          panel.background = element_rect(color = "black",fill = "white",size=1),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)) +  # Agrega una cuadrícula
    labs(x = "Tiempo", y = "Mediana de las distancias", title = paste("Concentración del", substr(r,2,3), "%")) +
    scale_fill_hue(labels = c("G1", "G2"))+
    guides(color = guide_legend(override.aes = list(shape = c("+", NA))))  # Personaliza la forma de los puntos en la leyenda
 
  ggsave(paste0(substr(r,2,3),".png"),graf, width = 9, height = 5, units = "in",dpi = 300)
  return(model$coefficients)
}

# Se carga la data y se procesa

d85 <- read_csv("/home/armandorg/Descargas/Fiji.app/85.csv")[-1]
d80 <- read_csv("/home/armandorg/Descargas/Fiji.app/80.csv")[-1]
d75 <- read_csv("/home/armandorg/Descargas/Fiji.app/75.csv")[-1]
d70 <- read_csv("/home/armandorg/Descargas/Fiji.app/70.csv")[-1]
d65 <- read_csv("/home/armandorg/Descargas/Fiji.app/65.csv")[-1]
d85[2:length(d85)]<-d85[2:length(d85)]*18.5166
d80[2:length(d80)]<-d80[2:length(d80)]*18.8453
d75[2:length(d75)]<-d75[2:length(d75)]*17.8575
d70[2:length(d70)]<-d70[2:length(d70)]*19.0230
d65[2:length(d65)]<-d65[2:length(d65)]*19.7288

tiempos<-c(60,60,40,60,50)
i<-1
for(k in c("d65","d70","d75","d80","d85")){
print(distime(get(k),tiempos[i],k))
i<-i+1
}

datos<-data.frame(Velocidad=c(1.635081,3.90646,4.507056,5.430748,7.407502 ), 
                  Concentración=c(65,70,75,80,85))

modeloo<-lm(datos$Velocidad~datos$Concentración)
predicciones <- predict(modeloo, interval = "confidence")

gr<-ggplot(datos, aes(x = Concentración, y = Velocidad)) +
  geom_point(color = "#2297E6",size=3,shape = 4) +  
  geom_line(aes(y = Velocidad),color = "#2297E6",size=1) + 
  geom_line(aes(y = predicciones[,1]), color = "red",size=1) +
  # geom_ribbon(aes(ymin = lower, ymax = upper),           # Intervalo de confianza
  #             fill = "lightblue", alpha = 0.5) +
  theme(legend.position = "bottom",  # Posición de la leyenda
        plot.title = element_text(hjust = 0.5,size = 16),  # Centra el título
        panel.grid.major = element_line(color = "gray"),
        panel.background = element_rect(color = "black",fill = "white",size=1),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +  # Agrega una cuadrícula
  labs(x = "Tiempo", y = "Mediana de las distancias", title = "Regresión Lineal de Velocidad vs Concentración, 25.4°C") +
  scale_fill_hue(labels = c("G1", "G2"))+
  guides(color = guide_legend(override.aes = list(shape = c("+", NA))))  # Personaliza la forma de los puntos en la leyenda

ggsave("vel_conc_ta.png",gr, width = 9, height = 5, units = "in",dpi = 300)













