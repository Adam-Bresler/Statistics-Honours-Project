data <- read.csv("BP_separated_with_H2H.csv")
data <- data[,-1]

features <- read.csv("features_with_H2H.csv")
features <- features[,-1]

raw_rolled              <- data[, c(1:15, 58:87,   288:317)]
raw_rolled_bc           <- data[, c(1:15, 100:129, 330:359)]
raw_weighted            <- data[, c(1:15, 142:171, 372:401)]
raw_weighted_bc         <- data[, c(1:15, 184:213, 414:443)]

raw_rolled              <- raw_rolled[,-c(17,29,47,59)]
raw_rolled_bc           <- raw_rolled_bc[,-c(17,29,47,59)]   
raw_weighted            <- raw_weighted[,-c(17,29,47,59)]  
raw_weighted_bc         <- raw_weighted_bc[,-c(17,29,47,59)]  


engineered_rolled       <- cbind.data.frame(data[, c(1:15, 230:233, 460:463)], features[,c(20, 16, 21:25, 41:46, 65:66, 73:74, 97)])
engineered_rolled_bc    <- cbind.data.frame(data[, c(1:15, 234:237, 464:467)], features[,c(20, 18, 26:30, 47:52, 67:68, 75:76, 98)])
engineered_weighted     <- cbind.data.frame(data[, c(1:15, 238:241, 468:471)], features[,c(20, 17, 31:35, 53:58, 69:70, 77:78, 97)])
engineered_weighted_bc  <- cbind.data.frame(data[, c(1:15, 242:245, 471:474)], features[,c(20, 19, 36:40, 59:64, 71:72, 79:80, 98)])


write.csv(raw_rolled, file = "C:/Users/bresl/Documents/GitHub/Statistics-Honours-Project/Data/raw_rolled.csv")
write.csv(raw_rolled_bc, file = "C:/Users/bresl/Documents/GitHub/Statistics-Honours-Project/Data/raw_rolled_bc.csv")
write.csv(raw_weighted, file = "C:/Users/bresl/Documents/GitHub/Statistics-Honours-Project/Data/raw_weighted.csv")
write.csv(raw_weighted_bc, file = "C:/Users/bresl/Documents/GitHub/Statistics-Honours-Project/Data/raw_weighted_bc.csv")

write.csv(engineered_rolled, file = "C:/Users/bresl/Documents/GitHub/Statistics-Honours-Project/Data/engineered_rolled.csv")
write.csv(engineered_rolled_bc, file = "C:/Users/bresl/Documents/GitHub/Statistics-Honours-Project/Data/engineered_rolled_bc.csv")
write.csv(engineered_weighted, file = "C:/Users/bresl/Documents/GitHub/Statistics-Honours-Project/Data/engineered_weighted.csv")
write.csv(engineered_weighted_bc, file = "C:/Users/bresl/Documents/GitHub/Statistics-Honours-Project/Data/engineered_weighted_bc.csv")

