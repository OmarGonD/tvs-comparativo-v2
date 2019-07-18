library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)



#start RSelenium


rD  <- rsDriver(port = 4575L, browser = "chrome", version = "latest", chromever = "75.0.3770.90",
                geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
                verbose = TRUE, check = TRUE)



remDr <- rD[["client"]]

#navigate to your page
remDr$navigate("https://www.lacuracao.pe/curacao/tv-y-audio/televisores")

#scroll down 6 times, waiting for the page to load at each time
for(i in 1:20){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(3)    
}

#get the page html
page_source<-remDr$getPageSource()


product_info <- function(node){
  precio_antes <- html_nodes(node, 'div.product_price span.old_price') %>% html_text
  precio_actual <- html_nodes(node, 'div.product_price span.price') %>% html_text 
  producto <- html_nodes(node,"div.product_name a") %>% html_text
  
  
  precio_antes <-   gsub("\\S\\/\\. ", "", precio_antes)
  precio_actual <-   gsub("\\S\\/\\. ", "", precio_actual)
  
  
  data.frame(
    ecommerce = "la-curacao",
    producto = producto,
    precio_antes = ifelse(length(precio_antes)==0, NA, precio_antes),
    precio_actual = ifelse(length(precio_actual)==0, NA, precio_actual), 
    stringsAsFactors=F
  )
  
  
}



doc <- read_html(iconv(page_source[[1]]), to="UTF-8") %>% 
  html_nodes("div.product_listing_container")



lacuracao_tvs <- lapply(doc, product_info) %>%
                  bind_rows()


str(lacuracao_tvs)

lacuracao_tvs$precio_antes <- gsub(",", "", lacuracao_tvs$precio_antes)
lacuracao_tvs$precio_actual <- gsub(",", "", lacuracao_tvs$precio_actual)


lacuracao_tvs$precio_antes <- gsub("S/", "", lacuracao_tvs$precio_antes)
lacuracao_tvs$precio_actual <- gsub("S/", "", lacuracao_tvs$precio_actual)
lacuracao_tvs$precio_actual <- gsub(" ", "", lacuracao_tvs$precio_actual)



trimws(" 1199.00")

lacuracao_tvs$precio_antes <- trimws(lacuracao_tvs$precio_antes, which = "both")
lacuracao_tvs$precio_actual <- stringr::str_trim((lacuracao_tvs$precio_actual))



lacuracao_tvs$precio_antes <- as.numeric(lacuracao_tvs$precio_antes)
lacuracao_tvs$precio_actual <- as.numeric(lacuracao_tvs$precio_actual)




lacuracao_tvs$producto <- gsub("â�???�", '"',lacuracao_tvs$producto)


##### Marca

lacuracao_tvs$marca <- ifelse(grepl("samsung", lacuracao_tvs$producto, ignore.case = T), "samsung",lacuracao_tvs$producto) 

lacuracao_tvs$marca <- ifelse(grepl("lg", lacuracao_tvs$producto, ignore.case = T), "lg",lacuracao_tvs$marca) 

lacuracao_tvs$marca <- ifelse(grepl("haier", lacuracao_tvs$producto, ignore.case = T), "haier",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("panasonic", lacuracao_tvs$producto, ignore.case = T), "panasonic",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("sony", lacuracao_tvs$producto, ignore.case = T), "sony",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("sharp", lacuracao_tvs$producto, ignore.case = T), "sharp",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("aoc", lacuracao_tvs$producto, ignore.case = T), "aoc",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("hisense", lacuracao_tvs$producto, ignore.case = T), "hisense",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("philips", lacuracao_tvs$producto, ignore.case = T), "philips",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("hyundai", lacuracao_tvs$producto, ignore.case = T), "hyundai",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("Orizon", lacuracao_tvs$producto, ignore.case = T), "orizon",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("imaco", lacuracao_tvs$producto, ignore.case = T), "imaco",lacuracao_tvs$marca)

lacuracao_tvs$marca <- ifelse(grepl("daewoo", lacuracao_tvs$producto, ignore.case = T), "daewoo",lacuracao_tvs$marca)

 



lacuracao_tvs$pulgadas <- sub(".*?(\\d+['\"]).*", "\\1", lacuracao_tvs$producto)




lacuracao_tvs$pulgadas <- sub('"', "", lacuracao_tvs$pulgadas)
lacuracao_tvs$pulgadas <- sub("'", "", lacuracao_tvs$pulgadas)


### Ordenar columnas

lacuracao_tvs <- lacuracao_tvs[,c(1,5,2,3,4,6)]


### To lower 


lacuracao_tvs <- as.data.frame(apply(lacuracao_tvs[,],2,tolower))



file <- paste(as.character(Sys.Date()), "lacuracao-tvs", sep = "-")

lacuracao_tvs_csv <- paste(file, "csv", sep = ".")


write.csv(lacuracao_tvs, lacuracao_tvs_csv, row.names = F)