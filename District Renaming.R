#District function 

function_dist_name <- function(Data,col){
  
   Data[col][Data[col] =="ARGHAKHACHI"] <- "ARGHAKHANCHI"
   Data[col][Data[col] =="KAVRE"] <- "KAVREPALANCHOK"
   Data[col][Data[col] =="SINDHUPALCHOWK"] <- "SINDHUPALCHOK"
   Data[col][Data[col] =="DOLKHA"] <- "DOLAKHA"
   Data[col][Data[col] =="DHANUSHA"] <- "DHANUSA"
   Data[col][Data[col] =="OKHALDHUNDA"] <- "OKHALDHUNGA"
   Data[col][Data[col] =="PANCHATHAR"] <- "PANCHTHAR"
   Data[col][Data[col] =="PARASI"] <- "PARSA"
   return(Data)
  
}



