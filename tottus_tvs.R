library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)



#start RSelenium


rD  <- rsDriver(port = 4522L, browser = "firefox", version = "latest", chromever = "76.0.3809.25",
                geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
                verbose = TRUE, check = TRUE)



remDr <- rD[["client"]]


tvs_url <- "https://www.tottus.com.pe/tottus/browse/Televisores/3.02"

remDr$navigate(tvs_url)

Sys.sleep(10)

#scroll down 20 times, waiting for the page to load at each time
for(i in 1:20){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(5)    
}


data = remDr$findElements(
  using = "xpath", 
  value = "/html/body/div[1]/div/div[3]/div/div/div/div/div/div/div/div/div"
)

rawData = sapply(
  X = data, 
  FUN = function(elem) elem$getElementText()
)



splitted = sapply(
  X = rawData, 
  FUN = strsplit, 
  split = "UN"
)



splitted_tvs = sapply(
  X = splitted, 
  FUN = strsplit, 
  split = "\n"
)



splitted_tvs_tvs <- splitted_tvs[3]


data <- splitted_tvs_tvs[[1]]


data <- data[lengths(data) == 5] 



length(data[[50]][1]) #if length is 5: product is 1, marca is 2, price is 5
length(data[[1]][[25]])  #if len is 4: prod is 1, pb is 2, pa is 3
length(data[[1]][[50]]) #if len is 3: prod is 1, pb is NA, pa is 2



#precios_splitted
#sapply(precios_splitted, "[", 1)
#sapply(precios_splitted, "[", 2)
#sapply(precios_splitted, "[", 3)



producto = ifelse(nchar(sapply(data, `[[`, 1))>20, sapply(data, "[", 1), sapply(data, "[", 2))
precios = sapply(data, "[", 5)
marca = ifelse(nchar(sapply(data, `[[`, 1))==0, sapply(data, "[", 3), sapply(data, "[", 2))

#de precios obtenemos precio_antes y precio_actual
precios_splitted <- strsplit(precios, split='S/', fixed=TRUE)

precio_antes = sapply(precios_splitted, "[", 2)
precio_actual = ifelse(lengths(precios_splitted)==3, sapply(precios_splitted, "[", 3), sapply(precios_splitted, "[", 2))

precio_antes

tottus_tvs <- data.frame(
  ecommerce = "tottus",
  marca = marca,
  producto = producto,
  precio_antes = precio_antes,
  precio_actual = precio_actual
  
)




tottus_tvs$precio_antes <- gsub(",", "", tottus_tvs$precio_antes)
tottus_tvs$precio_actual <- gsub(",", "", tottus_tvs$precio_actual)


tottus_tvs$precio_antes <- gsub("S/", "", tottus_tvs$precio_antes)
tottus_tvs$precio_actual <- gsub("S/", "", tottus_tvs$precio_actual)



tottus_tvs$precio_antes <- trimws(tottus_tvs$precio_antes, which = "both")
tottus_tvs$precio_actual <- trimws(tottus_tvs$precio_actual, which = "both")


tottus_tvs$precio_antes <- as.numeric(tottus_tvs$precio_antes)
tottus_tvs$precio_actual <- as.numeric(tottus_tvs$precio_actual)




tottus_tvs$pulgadas <- sub(".*?(\\d+['\"]).*", "\\1", tottus_tvs$producto)



tottus_tvs$pulgadas <- sub('"', "", tottus_tvs$pulgadas)
tottus_tvs$pulgadas <- sub("'", "", tottus_tvs$pulgadas)


tottus_tvs <- tottus_tvs[!(tottus_tvs$precio_actual<200),]


tottus_tvs <- as.data.frame(apply(tottus_tvs[,],2,tolower))


write.csv(tottus_tvs, "tottus-tvs.csv", row.names = F)





