library(tidyverse)
library(ggforce)
library(hrbrthemes)
library(scales)
library(tibble)
library(plotly)


Sys.Date()


tottus <- read_csv("2019-07-01-tottus-tvs.csv")

ripley <- read_csv("2019-07-01-ripley-tvs.csv")

falabella <- read_csv("falabella-tvs.csv")

lacuracao <- read_csv("lacuracao-tvs.csv")

wong <- read_csv("2019-07-01-wong-tvs.csv")

str(falabella)
str(ripley)

tvs <- bind_rows(falabella, ripley, lacuracao,
                 tottus, wong)

sum(is.na(tvs$precio_actual))
sum(is.na(tvs$precio_antes))

ecommerce_colors <- c("lacuracao" = "#ffda33","ripley" = "#802D69","falabella" = "#BED800",
                      'wong' = "red", "tottus" = "#fa3e14")



tvs_brand_colors <- c("hisense" = "#02c4aa","sony" = "#262626","panasonic" = "#FCB462",
  "samsung" = "#034ea2", "lg" = "#A21420", "philips" = "#428bff",
  "aoc" = "#9DCC27", "sharp" = "#BEBBDA", "hyundai" = "#FCB442",
  "haier" = "#919191")

##################################

# 
# tvs2016 <-  tvs2016 %>%
#   add_column(periodo = 2016, .before = "ecommerce")


tvs <- tvs %>%
  mutate(descuento = precio_actual/precio_antes - 1)


##########################################

tvs  <- tvs  %>%
  mutate(rango = ifelse(precio_actual <= 500, "< S/.500",
                         ifelse(precio_actual > 500 & precio_actual <= 1500,
                                "S/.500 - S/.1500",
                                ifelse(precio_actual > 1500 & precio_actual <= 2500,"S/.1500 - S/.2500",
                                       ifelse(precio_actual > 2500 & precio_actual <= 3500,"S/.2500 - S/.3500",
                                              ifelse(precio_actual > 3500 & precio_actual <= 4500,"S/.3500 - S/.4500",
                                                     "> S/.4,500"))))))



tvs$rangos <- factor(tvs$rangos, levels = c("< S/.500",
                                            "S/.500 - S/.1500",
                                            "S/.1500 - S/.2500",
                                            "S/.2500 - S/.3500",
                                            "S/.3500 - S/.4500",
                                            "> S/.4,500"),
                     ordered = T)


### Remover de Linio lo que no son TVs ###




tvs_cantidad <- tvs  %>%
  group_by(ecommerce) %>%
  summarise(cantidad = length(marca))



tvs_cantidad$ecommerce <- factor(tvs_cantidad$ecommerce, levels = tvs_cantidad$ecommerce[rev(order(tvs_cantidad$cantidad))])







tt1 <- "Ecommerce con más TVs"
stt1 <- ""
cptn <- "\nomar gonzales | Data Analyst"




ggplot(tvs_cantidad, aes(x=ecommerce, y= cantidad, fill = ecommerce)) + 
  geom_bar(stat = "identity", width = .7) +
  scale_fill_manual("ecommerce",
                    values = ecommerce_colors) +
  labs(title = "Ecommerce con más TVs\n",
       x = "", y = "") +
  theme_ipsum_rc(grid = "Y") +
  theme(axis.text.x = element_text(colour="grey10",size=20,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey10",size=14,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey40",size=6,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey40",size=6,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(size = 24,vjust=2,face="bold"),
        plot.subtitle = element_text(vjust=2, size = 16),
        plot.caption = element_text(vjust=2, size = 16),
        legend.position = "none",
        strip.text = element_text(size = 22, hjust = 0.5, vjust = -0.5)) +
  geom_text(aes(label=cantidad), vjust=-0.25, size = 6) +
  ylim(0, 200) +
  labs(title = tt1, subtitle = stt1, caption = cptn,
       x = "", y = "") 






ggsave(file="tvs-cantidades.jpg", width = 12, height = 8,
       path="imagenes")


### 

### falabella tiene una buena cantidad de TVs PRIMA y LEOTEC 

tvs_cantidad_por_marca <- tvs  %>%
  group_by(ecommerce, marca) %>%
  summarise(cantidad = length(marca))


###

tvs_rangos <- tvs %>%
             group_by(ecommerce, rango) %>%
             summarise(cantidad = length(rango))



unique(tvs_rango$ecommerce)


# 
# 
# tvs_rangos$periodo <- factor(tvs.rango$periodo, levels = c(2017,2016),
#                             ordered = T)









tvs_rangos$rango <- factor(tvs_rangos$rango, levels = c("< S/.500",
                                                      "S/.500 - S/.1500",
                                                      "S/.1500 - S/.2500",
                                                      "S/.2500 - S/.3500",
                                                      "S/.3500 - S/.4500",
                                                      "> S/.4,500"),
                          ordered = T)



# To use for fills, add

###


tt2 <- "Cantidad de TVs según rango de precio"
stt2 <- "\n"



ggplotly(ggplot(tvs_rangos, aes(x = rango, y = cantidad, fill = ecommerce)) +
           geom_bar(stat = "identity") + 
           scale_fill_manual("ecommerce",
                             values = ecommerce_colors) +
           # facet_grid(~ periodo) +
           coord_flip() +
           theme_ipsum_rc(grid = "X") +
           theme(axis.text.x = element_text(colour="grey10",size=12,hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(colour="grey10",size=14,,hjust=1,vjust=0,face="plain"),  
                 axis.title.x = element_text(colour="grey40",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                 axis.title.y = element_text(colour="grey40",size=16,angle=90,hjust=.5,vjust=.5,face="plain"),
                 plot.title = element_text(size = 24,vjust=4, face="bold"),
                 plot.subtitle = element_text(vjust=2, size = 16),
                 plot.caption = element_text(vjust=2, size = 16),
                 legend.title = element_text(colour="grey40", size=16, face="bold"),
                 legend.text = element_text(colour="grey10", size=16, face="bold"),
                 #strip.text.x = element_text(size = 14, angle = 0),
                 legend.position = "bottom",
                 strip.text = element_text(size = 22, hjust = .9, vjust = -.5)) +
           #geom_text(aes(label=cantidad), hjust=-0.25, size = 4) +
           ylim(0, 150) +
           labs(title = tt2, subtitle = stt2, caption = cptn,
                x = "", y = ""))










####


### Distribución de precios por marca



tvs_precios <- tvs %>%
  group_by(ecommerce, marca) %>%
  summarise(precio_actual = mean(precio_actual, na.rm = TRUE))


s <- plot_ly(tvs_precios, x = ~ecommerce, y = ~precio_actual, color = ~ecommerce, type = "box") %>%
  layout(boxmode = "group")

s


### Distribución de precios por Ecommerce

p <- plot_ly(tvs, x = ~ecommerce, y = ~precio_actual, color = ~ecommerce,
             colors = ~ecommerce_colors, type = "box", title = "Ecommerce Perú: Rango de precios") %>%
  layout(boxmode = "group")

p



###
### ripley ###
####




### Gr�ficos individuales por ecommerce ###




tvs_ripley <- tvs[tvs$ecommerce == "ripley",]

tvs_ripley_porcentajes <- tvs_ripley  %>%
  group_by(rango, marca) %>%
  summarise(c.marca = length(marca)) %>% 
  mutate(porcentaje_original = c.marca/sum(c.marca),
         porcentaje = sprintf("%.2f%%", c.marca/sum(c.marca) * 100))




tt3 <- "Ripley.com.pe \n % marcas de tvs por rango de precio"
stt3 <- "\n"






ggplotly(ggplot(tvs_ripley_porcentajes, aes(x=rango, y= porcentaje_original ,fill=marca, label=porcentaje)) + 
  geom_bar(stat = "identity", width = .7) +
  theme_ipsum_rc(grid = "Y") +
  coord_flip() +
  theme(axis.text.x = element_text(colour="grey20",size=12,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=10,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(vjust=2, size = 24,face="bold"),
        plot.subtitle = element_text(vjust=2, size = 16),
        plot.caption = element_text(vjust=2, size = 16),
        legend.position = "none",
        legend.box = "horizontal",
        legend.title=element_blank(),
        legend.text=element_text(size=18),
        strip.text = element_text(size = 22, hjust = 0, vjust = 2, face = "bold")) +
    
  scale_fill_manual(
    values = tvs_brand_colors) +
  labs(title = tt3, subtitle = stt3, caption = cptn,
       x = "", y = "") +
  scale_color_ipsum() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)),
  tooltip = c("label", "fill")) 



### Ripley - BoxPlot



#Qué tanto varían los precios, comparación antes y ahora
#La marca que más ha bajado sus precios es... 

tvs_ripley_variacion_precios <- tvs_ripley %>%
  group_by(marca, precio_antes, precio_actual) %>%
  mutate(dif_precios = precio_antes - precio_actual,
         dif_porcentual_original = round(dif_precios/precio_antes,2),
         dif_porcentual = sprintf("%.2f%%", round(dif_precios/precio_antes,2) * 100))





tt4 <- "Ripley - variación % descuentos según rango de precios"
stt4 <- "\n"



tvs_ripley_boxplot_variacion_precios <- plot_ly(tvs_ripley_variacion_precios, x = ~ecommerce, y = ~dif_porcentual, color = ~marca, type = "box") %>%
                                        layout(boxmode = "group")

tvs_ripley_boxplot_variacion_precios



# falabella 



### Gr�ficos individuales por ecommerce ###




tvs_falabella <- tvs[tvs$ecommerce == "falabella",]

tvs_falabella_porcentajes <- tvs_falabella  %>%
  group_by(rango, marca) %>%
  summarise(c.marca = length(marca)) %>% 
  mutate(porcentaje_original = c.marca/sum(c.marca),
         porcentaje = sprintf("%.2f%%", c.marca/sum(c.marca) * 100))




tt3 <- "falabella.com.pe \n % marcas de tvs por rango de precio"
stt3 <- "\n"






ggplotly(ggplot(tvs_falabella_porcentajes, aes(x=rango, y= porcentaje_original ,fill=marca, label=porcentaje)) + 
           geom_bar(stat = "identity", width = .7) +
           theme_ipsum_rc(grid = "Y") +
           coord_flip() +
           theme(axis.text.x = element_text(colour="grey20",size=12,hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(colour="grey20",size=10,hjust=1,vjust=0,face="plain"),  
                 axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
                 axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"),
                 plot.title = element_text(vjust=2, size = 24,face="bold"),
                 plot.subtitle = element_text(vjust=2, size = 16),
                 plot.caption = element_text(vjust=2, size = 16),
                 legend.position = "none",
                 legend.box = "horizontal",
                 legend.title=element_blank(),
                 legend.text=element_text(size=18),
                 strip.text = element_text(size = 22, hjust = 0, vjust = 2, face = "bold")) +
           
           scale_fill_manual(
             values = tvs_brand_colors) +
           labs(title = tt3, subtitle = stt3, caption = cptn,
                x = "", y = "") +
           scale_color_ipsum() +
           scale_y_continuous(labels = scales::percent_format(accuracy = 1)),
         tooltip = c("label", "fill")) 



### falabella - BoxPlot



#Qué tanto varían los precios, comparación antes y ahora
#La marca que más ha bajado sus precios es... 

tvs_falabella_variacion_precios <- tvs_falabella %>%
  group_by(marca, precio_antes, precio_actual) %>%
  mutate(dif_precios = precio_antes - precio_actual,
         dif_porcentual_original = round(dif_precios/precio_antes,2),
         dif_porcentual = sprintf("%.2f%%", round(dif_precios/precio_antes,2) * 100))





tt4 <- "falabella - variación % descuentos según rango de precios"
stt4 <- "\n"



tvs_falabella_boxplot_variacion_precios <- plot_ly(tvs_falabella_variacion_precios, x = ~ecommerce, y = ~dif_porcentual, color = ~marca, type = "box") %>%
  layout(boxmode = "group")

tvs_falabella_boxplot_variacion_precios


sum(is.na(tvs_falabella$precio_actual))
sum(is.na(tvs_falabella$precio_antes))

### Scaterplot Pulgadas vs Precios


tt10 <- "Precio de televisores según su tamaño (pulgadas)"
stt10 <- "\n"



str(tvs)





pulgadas_precio <- ggplot(tvs, aes(x = pulgadas, y = precio_actual)) + 
                    geom_point(aes(color=marca),size = 4,alpha = 0.6) +
                    facet_grid(~ ecommerce) +
                    theme_ipsum_rc(grid = "Y") +
                    theme(axis.text.x = element_text(colour="grey10",size=10,hjust=.5,vjust=.5,face="plain"),
                          axis.text.y = element_text(colour="grey10",size=10,hjust=1,vjust=0,face="plain"),  
                          axis.title.x = element_text(colour="grey40",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                          axis.title.y = element_text(colour="grey40",size=16,angle=90,hjust=.5,vjust=.5,face="plain"),
                          plot.title = element_text(size = 24,vjust=4, face="bold"),
                          plot.subtitle = element_text(vjust=2, size = 16),
                          plot.caption = element_text(vjust=2, size = 16),
                          legend.title = element_text(colour="grey40",size=14,hjust=.5,vjust=.5,face="bold"),
                          legend.text = element_text(colour="grey10", size=18, face="plain"),
                          strip.text.x = element_text(size = 18, angle = 0),
                          strip.text.y = element_text(size=14, face="bold"),
                          legend.position = "none") +
                    scale_y_continuous(label=comma, limits = c(0,50000)) +
                    scale_x_continuous(label=comma, limits = c(0,100)) +
                    labs(title = tt10, subtitle = stt10, caption = cptn,
                         x = "pulgadas \n", y = "precio en S/. \n") +
                    scale_color_discrete(name="marcas de tvs") +
                    geom_smooth()


ggplotly(pulgadas_precio)








### Write CSV con datos totales TVS


file <- paste(as.character(Sys.Date()),"total-tvs", sep = "-")

total.tvs.csv <- paste(file, "csv", sep = ".")


## 1er arg es l.tv // 2do arg es nombres del archivo

write.csv(tvs, total.tvs.csv, row.names = F)

