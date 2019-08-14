library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)



#start RSelenium



rD  <- rsDriver(port = 4572L, browser = "firefox", version = "latest", chromever = "latest",
                geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
                verbose = TRUE, check = TRUE)



remDr <- rD[["client"]]



Sys.sleep(10)

tvs_url <- "https://www.wong.pe/tecnologia/televisores/tv"

remDr$navigate(tvs_url)

Sys.sleep(10)

#scroll down 6 times, waiting for the page to load at each time
for(i in 1:16){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(5)    
}


data = remDr$findElements(
  using = "xpath", 
  value = "/html/body/div[24]/div/div[2]/div[6]/div[2]/div[2]/div[2]/div/ul/li/div"
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



#length(splitted[[15]]) #if length is 9, brand is 3, product is 2, price b is 5, p act is 8
#length(splitted[[5]])  #if len is 10, brand is 4, prod is 3, pb is 6, pa is 9
#length(splitted[[7]])  #if len is 11, brand is 5, prod is 4, pb is 7, pa is 10
#length(splitted[[27]]) #if len is 6, brand is 3, prod is 2, pb is NA, pa is 5
#length(splitted[[73]]) #if len is 5, brand is 2, prod is 1, pb is NA, pa is 4



#marca = ifelse(lengths(splitted)==9, sapply(splitted, "[", 3), ifelse(lengths(splitted)==10, sapply(splitted, "[", 4), ifelse(lengths(splitted)==6, sapply(splitted, "[", 3), ifelse(lengths(splitted)==5, sapply(splitted, "[", 2), sapply(splitted, "[", 5)))))
#producto = ifelse(lengths(splitted)==9, sapply(splitted, "[", 2), ifelse(lengths(splitted)==10, sapply(splitted, "[", 3), ifelse(lengths(splitted)==6, sapply(splitted, "[", 2), ifelse(lengths(splitted)==5, sapply(splitted, "[", 1), sapply(splitted, "[", 4)))))
#precio_antes = ifelse(lengths(splitted)==9, sapply(splitted, "[", 5), ifelse(lengths(splitted)==10, sapply(splitted, "[", 6), ifelse(lengths(splitted)==6, NA, ifelse(lengths(splitted)==5, NA, sapply(splitted, "[", 7)))))
#precio_actual = ifelse(lengths(splitted)==9, sapply(splitted, "[", 8), ifelse(lengths(splitted)==10, sapply(splitted, "[", 9), ifelse(lengths(splitted)==6, sapply(splitted, "[", 5), ifelse(lengths(splitted)==5, sapply(splitted, "[", 4), sapply(splitted, "[", 10)))))



wong_tvs <- data.frame(
  ecommerce = "wong",
  marca = ifelse(lengths(splitted)==9, sapply(splitted, "[", 3), ifelse(lengths(splitted)==10, sapply(splitted, "[", 4), ifelse(lengths(splitted)==6, sapply(splitted, "[", 3), ifelse(lengths(splitted)==5, sapply(splitted, "[", 2), sapply(splitted, "[", 5))))),
  producto = ifelse(lengths(splitted)==9, sapply(splitted, "[", 2), ifelse(lengths(splitted)==10, sapply(splitted, "[", 3), ifelse(lengths(splitted)==6, sapply(splitted, "[", 2), ifelse(lengths(splitted)==5, sapply(splitted, "[", 1), sapply(splitted, "[", 4))))),
  precio_antes = ifelse(lengths(splitted)==9, sapply(splitted, "[", 5), ifelse(lengths(splitted)==10, sapply(splitted, "[", 6), ifelse(lengths(splitted)==6, NA, ifelse(lengths(splitted)==5, NA, sapply(splitted, "[", 7))))),
  precio_actual = ifelse(lengths(splitted)==9, sapply(splitted, "[", 8), ifelse(lengths(splitted)==10, sapply(splitted, "[", 9), ifelse(lengths(splitted)==6, sapply(splitted, "[", 5), ifelse(lengths(splitted)==5, sapply(splitted, "[", 4), sapply(splitted, "[", 10)))))
  
)


wong_tvs <- wong_tvs[!is.na(wong_tvs$marca),]


wong_tvs$pulgadas <- sub(".*?(\\d+['\"]).*", "\\1", wong_tvs$producto)


wong_tvs$precio_antes <- trimws(wong_tvs$precio_antes, which = "both")

wong_tvs$precio_actual <- trimws(wong_tvs$precio_actual, which = "both")



wong_tvs$precio_antes <- gsub(",", "", wong_tvs$precio_antes)
wong_tvs$precio_actual <- gsub(",", "", wong_tvs$precio_actual)


wong_tvs$precio_antes <- gsub("S/", "", wong_tvs$precio_antes)
wong_tvs$precio_actual <- gsub("S/", "", wong_tvs$precio_actual)


wong_tvs$pulgadas <- sub('"', "", wong_tvs$pulgadas)
wong_tvs$pulgadas <- sub("'", "", wong_tvs$pulgadas)

wong_tvs$pulgadas <- as.character(wong_tvs$pulgadas)

wong_tvs$pulgadas <- sub("'", "", wong_tvs$pulgadas)


wong_tvs$pulgadas <- as.numeric(wong_tvs$pulgadas)

write.csv(wong_tvs, "wong-tvs.csv", row.names = F)





