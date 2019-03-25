library(dplyr)
library(ggplot2)
library(stargazer)
library(ggrepel)



# Assigning region to mexican states
# https://es.wikipedia.org/wiki/Regiones_de_M%C3%A9xico

NorOeste <- c('Baja California','Baja California Sur','Chihuahua','Durango','Sinaloa','Sonora')
NorEste <- c('Coahuila','Nuevo Leon','Tamaulipas')
Occidente <- c('Colima','Jalisco','Michoacan','Nayarit')
Oriente <- c('Hidalgo','Puebla','Tlaxcala','Veracruz')
CentroNorte <- c('Aguascalientes','Guanajuato','Queretaro','San Luis Potosi','Zacatecas')
CentroSur <- c('Mexico','Morelos','Distrito Federal')
SurOeste <- c('Chiapas','Guerrero','Oaxaca')
SurEste <- c('Campeche','Quintana Roo','Tabasco','Yucatan')


Region_Avocado <- Avocado %>% mutate(region = ifelse(Destino %in% NorOeste,"NW",
                                        ifelse(Destino %in% NorEste,"NE",
                                               ifelse(Destino %in% Occidente,"W",
                                                      ifelse(Destino %in% Oriente,"E",
                                                             ifelse(Destino %in% CentroNorte,"CN",
                                                                    ifelse(Destino %in% CentroSur,"CS",
                                                                           ifelse(Destino %in% SurOeste,"SW",
                                                                                  ifelse(Destino %in% SurEste,"SE",0)))))))))
  
  
Region_Tomato <- Tomato %>% mutate(region = ifelse(Destino %in% NorOeste,"NW",
                                                     ifelse(Destino %in% NorEste,"NE",
                                                            ifelse(Destino %in% Occidente,"W",
                                                                   ifelse(Destino %in% Oriente,"E",
                                                                          ifelse(Destino %in% CentroNorte,"CN",
                                                                                 ifelse(Destino %in% CentroSur,"CS",
                                                                                        ifelse(Destino %in% SurOeste,"SW",
                                                                                               ifelse(Destino %in% SurEste,"SE",0)))))))))

Region_Corn <- Corn %>% mutate(region = ifelse(Destino %in% NorOeste,"NW",
                                                     ifelse(Destino %in% NorEste,"NE",
                                                            ifelse(Destino %in% Occidente,"W",
                                                                   ifelse(Destino %in% Oriente,"E",
                                                                          ifelse(Destino %in% CentroNorte,"CN",
                                                                                 ifelse(Destino %in% CentroSur,"CS",
                                                                                        ifelse(Destino %in% SurOeste,"SW",
                                                                                               ifelse(Destino %in% SurEste,"SE",0)))))))))


mean_avo <- Avocado %>% group_by(year,Destino) %>% summarise(mean.price = mean(mPrice), sd.price = sd(mPrice))
mean_avo_region <- Region_Avocado %>% group_by(year,region) %>% summarise(mean.price = mean(mPrice), sd.price = sd(mPrice))
# write.xlsx(as.data.frame(mean_avo),'mean_avocado.xlsx',sheetName = 'Sheet1',append=TRUE)


mean_tom <- Tomato %>% group_by(year,Destino) %>% summarise(mean.price = mean(mPrice), sd.price = sd(mPrice))
mean_tom_region <- Region_Tomato %>% group_by(year,region) %>% summarise(mean.price = mean(mPrice), sd.price = sd(mPrice))
# write.xlsx(as.data.frame(mean_tom),'mean_tomato.xlsx',sheetName = 'Sheet1',append=TRUE)

mean_corn <- Corn %>% group_by(year,Destino) %>% summarise(mean.price = mean(mPrice), sd.price = sd(mPrice))
mean_corn_region <- Region_Corn %>% group_by(year,region) %>% summarise(mean.price = mean(mPrice), sd.price = sd(mPrice))
# write.xlsx(as.data.frame(mean_corn),'mean_corn.xlsx',sheetName = 'Sheet3',append=TRUE)

library(directlabels)

# TOMATO by region
png('tomato_by_region.png',width = 1000, height = 500)
ggplot(data=mean_tom_region, aes(x=year,y=mean.price, colour=region, label=region)) + geom_line(size=1.1) +
  geom_text_repel(
    data          = subset(mean_tom_region, year==2018),
    nudge_x       = 2019,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y"
  )+
guides(color = FALSE) + theme_bw() + 
  scale_x_discrete(name ="Year", 
                   limits=seq(1998,2018,1))+
  scale_y_continuous("Mean Price", breaks = seq(5,30,2)) +
  labs(title="Tomato", 
       subtitle="Mean price by region", 
       caption="Source: Own elaboration")
dev.off()

# CORN by region
png('corn_by_region.png',width = 1000, height = 500)
ggplot(data=mean_corn_region, aes(x=year,y=mean.price, colour=region, label=region)) + geom_line(size=1.1) +
  geom_text_repel(
    data          = subset(mean_corn_region, year==2018),
    nudge_x       = 2019,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y"
  )+
  guides(color = FALSE) + theme_bw() + 
  scale_x_discrete(name ="Year", 
                   limits=seq(1998,2018,1))+
  scale_y_continuous("Mean Price", breaks = seq(5,30,2)) +
  labs(title="Corn", 
       subtitle="Mean price by region", 
       caption="Source: Own elaboration")
dev.off()

# AVOCADO by region
png('avocado_by_region.png',width = 1000, height = 500)
ggplot(data=mean_avo_region, aes(x=year,y=mean.price, colour=region, label=region)) + geom_line(size=1.1) +
  geom_text_repel(
    data          = subset(mean_avo_region, year==2018),
    nudge_x       = 2019,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y"
  )+
  guides(color = FALSE) + theme_bw() + 
  scale_x_discrete(name ="Year", 
                   limits=seq(1998,2018,1))+
  scale_y_continuous("Mean Price", breaks = seq(5,30,2)) +
  labs(title="Avocado", 
       subtitle="Mean price by region", 
       caption="Source: Own elaboration")
dev.off()

# ggplot(data=Region_Corn, aes(x=year,y=mPrice, colour=Destino)) + geom_line(size=1.5) +
  # facet_wrap(.~region)

library(moments)

# first column: Kurtosis
# Second column: Skewness
Avocado_moments <- matrix(0,21,2)
Tomato_moments <- matrix(0,21,2)
Corn_moments <- matrix(0,21,2)
for(i in 1:21){
  Avocado_moments[i,1] <- Avocado  %>%  filter(year==1997+i) %>% select(mPrice) %>% kurtosis 
  Tomato_moments[i,1] <- Tomato  %>%  filter(year==1997+i) %>% select(mPrice) %>% kurtosis 
  Corn_moments[i,1] <- Corn  %>%  filter(year==1997+i) %>% select(mPrice) %>% kurtosis 
  
  Avocado_moments[i,2] <- Avocado  %>%  filter(year==1997+i) %>% select(mPrice) %>% skewness 
  Tomato_moments[i,2] <- Tomato  %>%  filter(year==1997+i) %>% select(mPrice) %>% skewness 
  Corn_moments[i,2] <- Corn  %>%  filter(year==1997+i) %>% select(mPrice) %>% skewness 
}

plot(Avocado_moments[,1], col='red',type="b",lwd=2,main="Kurtosis", ylim = c(0,25),xlab = "year",pch=1)
points(Tomato_moments[,1], col='blue',type="b",lty=2,lwd=2,pch=2)
points(Corn_moments[,1], col='magenta',type="b",lty=3,lwd=2,pch=3)

legend("topleft",legend = c('Avocado','Tomato','Corn'),col = c('red','blue','magenta'),lty=1:3,pch=1:3)

plot(Avocado_moments[,2], col='red',type="b",lwd=2,main="Skewness", ylim = c(-5,10),xlab = "year",pch=1)
points(Tomato_moments[,2], col='blue',type="b",lty=2,lwd=2,pch=2)
points(Corn_moments[,2], col='magenta',type="b",lty=3,lwd=2,pch=3)

legend("topleft",legend = c('Avocado','Tomato','Corn'),col = c('red','blue','magenta'),lty=1:3,pch=1:3)
