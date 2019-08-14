library(tidyverse)
library(plotly)
library(repmis)




tvs_2016_2017 <- source_data("https://www.dropbox.com/s/6arewitgenhwwba/2017-03-15-total-tvs.csv?raw=1")

str(tvs_2016_2017)


colnames(tvs_2016_2017)[5] = "precio_antes"
colnames(tvs_2016_2017)[6] = "precio_actual"
colnames(tvs_2016_2017)[8] = "rangos"


tvs_2016_2017$descuento <- NULL

###

ripley_tvs <- read_csv("ripley-tvs.csv")
falabella_tvs <- read_csv("falabella-tvs.csv")
linio_tvs <- read_csv("linio-tvs.csv")
wong_tvs <- read_csv("wong-tvs.csv")
lacuracao_tvs <- read_csv("lacuracao-tvs.csv")
tottus_tvs <- read_csv("tottus-tvs.csv")

tvs_2019 <- rbind(ripley_tvs, falabella_tvs, linio_tvs,
             wong_tvs, lacuracao_tvs, tottus_tvs) 

tvs_2019$periodo <- 2019



tvs_2019 <- tvs_2019[,c(7,1,2,3,4,5,6)]

###

colnames(tvs_2016_2017)
colnames(tvs_2019)


str(tvs_2019)

tvs_2019$precio_antes <- as.numeric(tvs_2019$precio_antes)
tvs_2019$precio_actual <- as.numeric(tvs_2019$precio_actual)


tvs_2019  <- tvs_2019  %>%
  mutate(rangos = ifelse(precio_actual <= 500, "< S/.500",
                         ifelse(precio_actual > 500 & precio_actual <= 1500,
                                "S/.500 - S/.1500",
                                ifelse(precio_actual > 1500 & precio_actual <= 2500,"S/.1500 - S/.2500",
                                       ifelse(precio_actual > 2500 & precio_actual <= 3500,"S/.2500 - S/.3500",
                                              ifelse(precio_actual > 3500 & precio_actual <= 4500,"S/.3500 - S/.4500",
                                                     "> S/.4,500"))))))

tvs <- rbind(tvs_2016_2017, tvs_2019)

###


str(tvs)

tvs$precio_antes <- as.numeric(tvs$precio_antes)
tvs$precio_actual <- as.numeric(tvs$precio_actual)


unique(tvs$rangos)

tvs$rangos <- factor(tvs$rangos, levels = c("< S/.500",
                                            "S/.500 -\r\n S/.1500",
                                            "S/.1500 -\r\n S/.2500",
                                            "S/.2500 -\r\n S/.3500",
                                            "S/.3500 -\r\n S/.4500",
                                            "> S/.4,500"),
                     ordered = T)


write.csv(tvs, "tvs-2019.csv", row.names = FALSE)


###



brand_tvs_colors <- c("hisense" = "#F39EF7", "sony" = "#003366","panasonic" = "#FCB462",
                      "samsung" = "#7ec0ee", "lg" = "#A21420",
                      "aoc" = "#9DCC27", "sharp" = "#BEBBDA", "hyundai" = "#FCB442",
                      "haier" = "#454545", "imaco" = "#e6f029", "royal" = "#cb7dff",
                      "olitec" = "#1472c4", "nex" = "#d3d9de", "daewoo" = "#333333",
                      "altron" = "#e69729", "blackline" = "#87765f", "miray" = "#63b33b",
                      "king master" = "#19bfba", "xenon" = "#a640bd", "jvc" = "#d19b90")




ecommerce_colors <- c("linio" = "#ff5500","ripley" = "#802D69","falabella" = "#aad500",
                      "wong" = "#ec1c24", "tottus" = "#76b703", "lacuracao" = "#ffec02")




## TVs por Ecommerce




############################################
### Total TVS 2017 vs 2016 por Ecommerce ###
############################################



### Remover de Linio lo que no son TVs #####



tvs <- tvs %>%
  filter(pulgadas < 90, !is.na(marca))


tvs_cantidad <- tvs  %>%
  group_by(periodo, ecommerce) %>%
  summarise(cantidad = length(marca))


str(tvs_cantidad)


tvs_cantidad <- tvs_cantidad %>%
  group_by(ecommerce) %>% 
  mutate(diferencia_porcentual = (cantidad - lag(cantidad))/lag(cantidad))

str(tvs_cantidad)

tvs_cantidad[is.na(tvs_cantidad)] <- 0







tvs_cantidad$ecommerce <- factor(tvs_cantidad$ecommerce, levels = c("linio",
                                                                    "ripley",
                                                                    "falabella",
                                                                    "tottus",
                                                                    "wong",
                                                                    "lacuracao"),
                                 ordered = T)







tvs_cantidad$periodo <- factor(tvs_cantidad$periodo, levels = c(2016,2017, 2019),
                               ordered = T)

tt1 <- ""
stt1 <- ""
# tt1 <- "Ecommerce con más TVs"
# stt1 <- "Linio, Ripley y Falabella son los 3 principales ecommerce del Perú. \n"
cptn <- "\nogonzales.com | Data Analyst"





cantidad_tvs <- ggplot(tvs_cantidad, aes(x=ecommerce, y= cantidad, fill = ecommerce)) + 
  geom_bar(stat = "identity", width = .7) +
  facet_wrap(~ periodo, ncol = 2) +
  theme_bw() +
  scale_fill_manual("ecommerce",
                    values = ecommerce_colors) +
  labs(title = "Ecommerce con más TVs\n",
       x = "", y = "") +
  #theme_ipsum_rc(grid = "Y") +
  theme(axis.text.x = element_text(colour="grey10",size=12,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey10",size=8,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey40",size=6,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey40",size=6,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(size = 24,vjust=2,face="bold"),
        plot.subtitle = element_text(vjust=2, size = 16),
        plot.caption = element_text(vjust=2, size = 8),
        panel.border = element_rect(colour = "white"),
        legend.position = "none",
        strip.text = element_text(size = 18, hjust = 0.08, vjust = -0.5),
        strip.background = element_rect(colour = "white", fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  #geom_text(aes(label=cantidad), vjust=-0.6, size = 4) +
  ylim(0, 600)



ggplotly(cantidad_tvs, tooltip=c("cantidad"))  %>% 
  config(displayModeBar = FALSE)


#####
#####





unique(tvs$rangos)


tvs_rangos <- tvs %>%
  group_by(periodo, ecommerce, rangos) %>%
  summarise(cantidad = length(rangos))

unique(tvs_2019$rangos)
unique(tvs$rangos)
unique(tvs_rangos$rangos)


tvs_rangos$periodo <- factor(tvs_rangos$periodo, levels = c(2016,2017, 2019),
                             ordered = T)



tvs_cantidad$ecommerce <- factor(tvs_cantidad$ecommerce, levels = c("linio",
                                                                    "ripley",
                                                                    "falabella",
                                                                    "tottus",
                                                                    "wong",
                                                                    "lacuracao"),
                                 ordered = T)





tvs_rangos$rangos <- factor(tvs_rangos$rangos, levels = c("< S/.500",
                                                          "S/.500 -\r\n S/.1500",
                                                          "S/.1500 -\r\n S/.2500",
                                                          "S/.2500 -\r\n S/.3500",
                                                          "S/.3500 -\r\n S/.4500",
                                                          "> S/.4,500"),
                            ordered = T)



# To use for fills, add

###

### No usar t�tulo porque no entra en los l�mites del
### formato de la p�gina web. 


#tt2 <- "Cantidad de TVs seg�n rangos de precio"
tt2 <- ""
stt2 <- "\n"



tvs_por_rangos_de_precio <- ggplotly(ggplot(tvs_rangos, aes(x = rangos, y = cantidad, fill = ecommerce)) +
                                       geom_bar(stat = "identity") + 
                                       scale_fill_manual("ecommerce",
                                                         values = ecommerce_colors) +
                                       facet_grid(~ periodo) +
                                       theme_bw() +
                                       coord_flip() +
                                       #theme_ipsum_rc(grid = "X") +
                                       theme(axis.text.x = element_text(colour="grey10",size=10,hjust=.5,vjust=.5,face="plain"),
                                             axis.text.y = element_text(colour="grey10",size=10,hjust=1,vjust=0,face="plain"),  
                                             axis.title.x = element_text(colour="grey40",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                             axis.title.y = element_text(colour="grey40",size=16,angle=90,hjust=.5,vjust=.5,face="plain"),
                                             plot.title = element_text(size = 24,vjust=4, face="bold"),
                                             plot.subtitle = element_text(vjust=2, size = 16),
                                             plot.caption = element_text(vjust=2, size = 16),
                                             panel.border = element_rect(colour = "white"),
                                             legend.position = "none",
                                             strip.text = element_text(size = 18, hjust = 0.01, vjust = -0.5),
                                             strip.background = element_rect(colour = "white", fill = "white"),
                                             panel.grid.major.y = element_blank(),
                                             panel.grid.minor.y = element_blank()) +
                                       #geom_text(aes(label=cantidad), hjust=-0.25, size = 4) +
                                       ylim(0, 300) +
                                       labs(title = tt2, subtitle = stt2, caption = cptn,
                                            x = "", y = ""),
                                     tooltip=c("cantidad", "ecommerce"))



tvs_por_rangos_de_precio %>% 
  config(displayModeBar = FALSE)
