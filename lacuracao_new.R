library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)



#start RSelenium


rD  <- rsDriver(port = 4582L, browser = "chrome", version = "latest", chromever = "75.0.3770.90",
                geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
                verbose = TRUE, check = TRUE)



remDr <- rD[["client"]]


Sys.sleep(10)

tvs_url <- "https://www.lacuracao.pe/curacao/tv-y-audio/televisores"

remDr$navigate(tvs_url)

Sys.sleep(10)

#scroll down 20 times, waiting for the page to load at each time
for(i in 1:20){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(5)    
}


data = remDr$findElements(
  using = "xpath", 
  value = "/html/body/div[3]/div[3]/div/div/div[3]/div[2]/div[2]/div[1]/div[2]/ul/li"
)

rawData = sapply(
  X = data, 
  FUN = function(elem) elem$getElementText()
)

splitted = sapply(
  X = rawData, 
  FUN = strsplit, 
  split = "\n"
)

splitted[[15]]
splitted[[7]]
splitted[[21]]
splitted[[12]]

length(splitted[[15]]) #if length is 5: product is 2, price b is 3, p act is 4
length(splitted[[7]])  #if len is 4: prod is 1, pb is 2, pa is 3
length(splitted[[21]]) #if len is 3: prod is 1, pb is NA, pa is 2


producto = ifelse(lengths(splitted)==3, sapply(splitted, "[", 1), ifelse(lengths(splitted)==4,sapply(splitted, "[", 1),sapply(splitted, "[", 2)))
precio_antes = ifelse(lengths(splitted)==3, NA, ifelse(lengths(splitted)==4, sapply(splitted, "[", 2), sapply(splitted, "[", 3)))
precio_actual = ifelse(lengths(splitted)==3, sapply(splitted, "[", 2), ifelse(lengths(splitted)==4, sapply(splitted, "[", 3), sapply(splitted, "[", 4)))

producto

lacuracao_tvs <- data.frame(
  ecommerce = "lacuracao",
  marca = "por obtener",
  producto = ifelse(lengths(splitted)==3, sapply(splitted, "[", 1), ifelse(lengths(splitted)==4,sapply(splitted, "[", 1),sapply(splitted, "[", 2))),
  precio_antes = ifelse(lengths(splitted)==3, NA, ifelse(lengths(splitted)==4, sapply(splitted, "[", 2), sapply(splitted, "[", 3))),
  precio_actual = ifelse(lengths(splitted)==3, sapply(splitted, "[", 2), ifelse(lengths(splitted)==4, sapply(splitted, "[", 3), sapply(splitted, "[", 4)))
  
)


##### Marca

lacuracao_tvs$marca <- ifelse(grepl("samsung", lacuracao_tvs$producto, ignore.case = T), "samsung",lacuracao_tvs$producto) 

lacuracao_tvs$marca <- ifelse(grepl("lg", lacuracao_tvs$producto, ignore.case = T), "lg",lacuracao_tvs$marca) 

lacuracao_tvs$marca <- ifelse(grepl("haier", lacuracao_tvs$producto, ignore.case = T), "haier",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("panasonic", lacuracao_tvs$producto, ignore.case = T), "panasonic",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("sony", lacuracao_tvs$producto, ignore.case = T), "sony",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("sharp", lacuracao_tvs$producto, ignore.case = T), "sharp",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("aoc", lacuracao_tvs$producto, ignore.case = T), "aoc",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("philips", lacuracao_tvs$producto, ignore.case = T), "philips",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("hyundai", lacuracao_tvs$producto, ignore.case = T), "hyundai",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("hisense", lacuracao_tvs$producto, ignore.case = T), "hisense",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("orizon", lacuracao_tvs$producto, ignore.case = T), "orizon",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("imaco", lacuracao_tvs$producto, ignore.case = T), "imaco",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("daewoo", lacuracao_tvs$producto, ignore.case = T), "daewoo",lacuracao_tvs$marca)




lacuracao_tvs$precio_antes <- gsub(",", "", lacuracao_tvs$precio_antes)
lacuracao_tvs$precio_actual <- gsub(",", "", lacuracao_tvs$precio_actual)


lacuracao_tvs$precio_antes <- gsub("S/", "", lacuracao_tvs$precio_antes)
lacuracao_tvs$precio_actual <- gsub("S/", "", lacuracao_tvs$precio_actual)



lacuracao_tvs$precio_antes <- trimws(lacuracao_tvs$precio_antes, which = "both")
lacuracao_tvs$precio_actual <- trimws(lacuracao_tvs$precio_actual, which = "both")


lacuracao_tvs$precio_antes <- as.numeric(lacuracao_tvs$precio_antes)
lacuracao_tvs$precio_actual <- as.numeric(lacuracao_tvs$precio_actual)



lacuracao_tvs$pulgadas <- sub(".*?(\\d+['\"]).*", "\\1", lacuracao_tvs$producto)



lacuracao_tvs$pulgadas <- sub('"', "", lacuracao_tvs$pulgadas)
lacuracao_tvs$pulgadas <- sub("'", "", lacuracao_tvs$pulgadas)


lacuracao_tvs$pulgadas <- as.character(lacuracao_tvs$pulgadas)

lacuracao_tvs <- lacuracao_tvs[nchar(lacuracao_tvs$pulgadas)==2,]

lacuracao_tvs$pulgadas <- as.numeric(lacuracao_tvs$pulgadas)


lacuracao_tvs$precio_antes <- ifelse(is.na(lacuracao_tvs$precio_antes), lacuracao_tvs$precio_actual, lacuracao_tvs$precio_antes) 




str(lacuracao_tvs)


lacuracao_tvs <- lacuracao_tvs[!(lacuracao_tvs$precio_actual<100),]


lacuracao_tvs <- as.data.frame(apply(lacuracao_tvs[,],2,tolower))


# file <- paste(as.character(Sys.Date()), "lacuracao-tvs", sep = "-")

# lacuracao_tvs_csv <- paste(file, "csv", sep = ".")

lacuracao_tvs_csv <- "lacuracao-tvs.csv"

write.csv(lacuracao_tvs, lacuracao_tvs_csv, row.names = F)





