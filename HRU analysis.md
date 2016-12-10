# HRU-analysis



library(data.table)
library(ggplot2)
library(hydroTSM)
library(tidyr)


setwd("F:\\HSE\\Thesis")
hru.des <- data.frame(read.csv2("HRU.txt", header = TRUE, sep = ";"))
#hru.des <- read.csv2("HRU.txt", header = FALSE, sep = "", skip = 1)
#setwd("D:\\Jinghe_River\\0919_ET_LU")



dt.hru.tot <- read.csv2("HRU_dat.hru", header = FALSE, skip = 9, sep = "", dec = ".", as.is = FALSE, numerals = "allow.loss")

dt.hru.yrly <- dt.hru.tot[c(28381:30745, 59126:61490, 89871:92235, 
                            120616:122980,151361:153725,182106:184470 ),]
dt.hru.yrly <- separate(dt.hru.yrly, V6, into = c("Mon", "Area"), sep = 4, convert = TRUE)
dt.hru.yrly <- subset(dt.hru.yrly, select = -c(V1:V5, Area))
colnames(dt.hru.yrly) <- c("YEAR", "ET", "PER", "GW_RCHG")
hru.yr <- as.data.table(cbind(hru.des, dt.hru.yrly))



hru.yr.sb1 <- hru.yr[hru.yr$SUBBASIN == 1, ]
hru.yr.sb2 <- hru.yr[hru.yr$SUBBASIN == 2, ]
hru.yr.sb3 <- hru.yr[hru.yr$SUBBASIN == 3, ]
hru.yr.sb4 <- hru.yr[hru.yr$SUBBASIN == 4, ]
hru.yr.sb5 <- hru.yr[hru.yr$SUBBASIN == 5, ]
hru.yr.sb6 <- hru.yr[hru.yr$SUBBASIN == 6, ]
hru.yr.sb7 <- hru.yr[hru.yr$SUBBASIN == 7, ]
hru.yr.sb8 <- hru.yr[hru.yr$SUBBASIN == 8, ]
hru.yr.sb9 <- hru.yr[hru.yr$SUBBASIN == 9, ]
hru.yr.sb10 <- hru.yr[hru.yr$SUBBASIN == 10, ]
hru.yr.sb11<- hru.yr[hru.yr$SUBBASIN == 11, ]
hru.yr.sb12<- hru.yr[hru.yr$SUBBASIN == 12, ]
hru.yr.sb13<- hru.yr[hru.yr$SUBBASIN == 13, ]
hru.yr.sb14<- hru.yr[hru.yr$SUBBASIN == 14, ]
hru.yr.sb15<- hru.yr[hru.yr$SUBBASIN == 15, ]
hru.yr.sb16<- hru.yr[hru.yr$SUBBASIN == 16, ]
hru.yr.sb17<- hru.yr[hru.yr$SUBBASIN == 17, ]
hru.yr.sb18<- hru.yr[hru.yr$SUBBASIN == 18, ]
hru.yr.sb19<- hru.yr[hru.yr$SUBBASIN == 19, ]
hru.yr.sb20<- hru.yr[hru.yr$SUBBASIN == 20, ]
hru.yr.sb21<- hru.yr[hru.yr$SUBBASIN == 21, ]
hru.yr.sb22<- hru.yr[hru.yr$SUBBASIN == 22, ]
hru.yr.sb23<- hru.yr[hru.yr$SUBBASIN == 23, ]
hru.yr.sb24<- hru.yr[hru.yr$SUBBASIN == 24, ]
hru.yr.sb25<- hru.yr[hru.yr$SUBBASIN == 25, ]
hru.yr.sb26<- hru.yr[hru.yr$SUBBASIN == 26, ]
hru.yr.sb27<- hru.yr[hru.yr$SUBBASIN == 27, ]
hru.yr.sb28<- hru.yr[hru.yr$SUBBASIN == 28, ]
hru.yr.sb29<- hru.yr[hru.yr$SUBBASIN == 29, ]
hru.yr.sb30<- hru.yr[hru.yr$SUBBASIN == 30, ]
hru.yr.sb31<- hru.yr[hru.yr$SUBBASIN == 31, ]
hru.yr.sb32<- hru.yr[hru.yr$SUBBASIN == 32, ]
hru.yr.sb33<- hru.yr[hru.yr$SUBBASIN == 33, ]
hru.yr.sb34<- hru.yr[hru.yr$SUBBASIN == 34, ]
hru.yr.sb35<- hru.yr[hru.yr$SUBBASIN == 35, ]
hru.yr.sb36<- hru.yr[hru.yr$SUBBASIN == 36, ]
hru.yr.sb37<- hru.yr[hru.yr$SUBBASIN == 37, ]
hru.yr.sb38<- hru.yr[hru.yr$SUBBASIN == 38, ]
hru.yr.sb39<- hru.yr[hru.yr$SUBBASIN == 39, ]
hru.yr.sb40<- hru.yr[hru.yr$SUBBASIN == 40, ]
hru.yr.sb41<- hru.yr[hru.yr$SUBBASIN == 41, ]
hru.yr.sb42<- hru.yr[hru.yr$SUBBASIN == 42, ]
hru.yr.sb43<- hru.yr[hru.yr$SUBBASIN == 43, ]
hru.yr.sb44<- hru.yr[hru.yr$SUBBASIN == 44, ]
hru.yr.sb45<- hru.yr[hru.yr$SUBBASIN == 45, ]
hru.yr.sb46<- hru.yr[hru.yr$SUBBASIN == 46, ]
hru.yr.sb47<- hru.yr[hru.yr$SUBBASIN == 47, ]
hru.yr.sb48<- hru.yr[hru.yr$SUBBASIN == 48, ]
hru.yr.sb49<- hru.yr[hru.yr$SUBBASIN == 49, ]
hru.yr.sb50<- hru.yr[hru.yr$SUBBASIN == 50, ]
hru.yr.sb51<- hru.yr[hru.yr$SUBBASIN == 51, ]
hru.yr.sb52<- hru.yr[hru.yr$SUBBASIN == 52, ]
hru.yr.sb53<- hru.yr[hru.yr$SUBBASIN == 53, ]
hru.yr.sb54<- hru.yr[hru.yr$SUBBASIN == 54, ]
hru.yr.sb55<- hru.yr[hru.yr$SUBBASIN == 55, ]
hru.yr.sb56<- hru.yr[hru.yr$SUBBASIN == 56, ]
hru.yr.sb57<- hru.yr[hru.yr$SUBBASIN == 57, ]
hru.yr.sb58<- hru.yr[hru.yr$SUBBASIN == 58, ]
hru.yr.sb59<- hru.yr[hru.yr$SUBBASIN == 59, ]
hru.yr.sb60<- hru.yr[hru.yr$SUBBASIN == 60, ]
hru.yr.sb61<- hru.yr[hru.yr$SUBBASIN == 61, ]
hru.yr.sb62<- hru.yr[hru.yr$SUBBASIN == 62, ]
hru.yr.sb63<- hru.yr[hru.yr$SUBBASIN == 63, ]
hru.yr.sb64<- hru.yr[hru.yr$SUBBASIN == 64, ]
hru.yr.sb65<- hru.yr[hru.yr$SUBBASIN == 65, ]

hru.yr.mnt <- data.table(rbind(hru.yr.sb1, hru.yr.sb2,hru.yr.sb4, hru.yr.sb6, hru.yr.sb7,hru.yr.sb8, hru.yr.sb9, hru.yr.sb10, 
                               hru.yr.sb16, hru.yr.sb17, hru.yr.sb29, hru.yr.sb33, hru.yr.sb34, hru.yr.sb35, hru.yr.sb36, hru.yr.sb41, 
                               hru.yr.sb42, hru.yr.sb43, hru.yr.sb46, hru.yr.sb47,hru.yr.sb50, hru.yr.sb53, hru.yr.sb60, hru.yr.sb61, hru.yr.sb65 ))


hru.yr.mnt <- hru.yr.mnt[hru.yr.mnt$LANDUSE != c("ROCK" ),]
hru.yr.mnt <- hru.yr.mnt[hru.yr.mnt$LANDUSE != c("URBN"),]
hru.yr.mnt <- hru.yr.mnt[hru.yr.mnt$LANDUSE != c("WATR"),]
hru.yr.mnt <- hru.yr.mnt[hru.yr.mnt$LANDUSE != c("WETN"),]


hru.yr.lss <- data.table(rbind(hru.yr.sb3, hru.yr.sb5, hru.yr.sb11, hru.yr.sb12, hru.yr.sb13, hru.yr.sb14, hru.yr.sb15, hru.yr.sb18, hru.yr.sb19, 
                               hru.yr.sb20,hru.yr.sb21, hru.yr.sb22, hru.yr.sb23, hru.yr.sb24, hru.yr.sb25, hru.yr.sb26, hru.yr.sb27, hru.yr.sb28, 
                               hru.yr.sb30, hru.yr.sb31, hru.yr.sb32, hru.yr.sb37, hru.yr.sb38, hru.yr.sb39, hru.yr.sb40, hru.yr.sb44, hru.yr.sb45, 
                               hru.yr.sb48, hru.yr.sb49,hru.yr.sb51, hru.yr.sb52, hru.yr.sb54, hru.yr.sb55, hru.yr.sb56, hru.yr.sb57, hru.yr.sb58, 
                               hru.yr.sb59, hru.yr.sb62, hru.yr.sb63, hru.yr.sb64 ))

hru.yr.lss <- hru.yr.lss[hru.yr.lss$LANDUSE != c("ROCK" ),]
hru.yr.lss <- hru.yr.lss[hru.yr.lss$LANDUSE != c("URBN"),]
hru.yr.lss <- hru.yr.lss[hru.yr.lss$LANDUSE != c("WATR"),]
hru.yr.lss <- hru.yr.lss[hru.yr.lss$LANDUSE != c("WETN"),]


hru.yr.mnt.avr <- hru.yr.mnt [ , list(avg.ET = mean(ET)) , by = c("YEAR", "LANDUSE", "SOIL", "SLOPE_CD")  ]
hru.yr.lss.avr <- hru.yr.lss [ , list(avg.ET = mean(ET)) , by = c("YEAR", "LANDUSE", "SOIL", "SLOPE_CD")  ]

write.table(hru.yr.mnt.avr, file = "D:\\Jinghe_River\\Analysis_scripts\\hru_munt_yr.csv", sep = ";")
write.table(hru.yr.lss.avr, file = "D:\\Jinghe_River\\Analysis_scripts\\hru_lss_yr.csv", sep = ";")



hru.mnt.avr <- hru.yr.mnt [ , list(avg.ET = mean(ET), se = sd(ET)/sqrt(length(ET)), sd= sd(ET)) , by = c("SOIL", "LANDUSE",  "SLOPE_CD")  ]
hru.lss.avr <- hru.yr.lss [ , list(avg.ET = mean(ET), se = sd(ET)/sqrt(length(ET)), sd= sd(ET)) , by = c("SOIL", "LANDUSE", "SLOPE_CD")  ]

ggplot(data = hru.mnt.avr, mapping = aes(x= LANDUSE, y=avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity", position = "dodge") + facet_grid(SOIL ~ .)+ geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))

ggplot(data = hru.lss.avr, mapping = aes(x= LANDUSE, y=avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity", position = "dodge") + facet_grid(SOIL ~ .) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))

hru.mnt.cc <- hru.mnt.avr[ SOIL == "Calcic Chernozems"]
ggplot(data = hru.mnt.cc,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Chernozems in Mountaneous HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET - se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.mnt.cc, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Chernozems in Mountaneous HRU level")
ggplot(data = hru.mnt.cc, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Chernozems in Mountaneous HRU level")


hru.mnt.ck <- hru.mnt.avr[ SOIL == "Calcic Kastanozems"]
ggplot(data = hru.mnt.ck,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Kastanozems in Mountaneous HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.mnt.ck, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Kastanozems in Mountaneous HRU level")
ggplot(data = hru.mnt.ck, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Kastanozems in Mountaneous HRU level")


hru.mnt.cl <- hru.mnt.avr[ SOIL == "Calcic Luvisols"]
ggplot(data = hru.mnt.cl,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Luvisols in Mountaneous HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.mnt.cl, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Luvisols in Mountaneous HRU level")
ggplot(data = hru.mnt.cl, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Luvisols in Mountaneous HRU level")


  
hru.mnt.fsc <- hru.mnt.avr[ SOIL == "Folic stagnic Cambisols"]
ggplot(data = hru.mnt.fsc,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Folic stagnic Cambisols in Mountaneous HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.mnt.fsc, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Folic stagnic Cambisols in Mountaneous HRU level")
ggplot(data = hru.mnt.fsc, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Folic stagnic Cambisols in Mountaneous HRU level")


hru.mnt.hf <- hru.mnt.avr[ SOIL == "Haplic Fluvisols"]
ggplot(data = hru.mnt.hf,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Fluvisols in Mountaneous HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.mnt.hf, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Fluvisols in Mountaneous HRU level")
ggplot(data = hru.mnt.hf, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Fluvisols in Mountaneous HRU level")


hru.mnt.hl <- hru.mnt.avr[ SOIL == "Haplic Luvisols"]
ggplot(data = hru.mnt.hl,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Luvisols in Mountaneous HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.mnt.hl, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Luvisols in Mountaneous HRU level")
ggplot(data = hru.mnt.hl, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Luvisols in Mountaneous HRU level")


hru.mnt.hr <- hru.mnt.avr[ SOIL == "Haplic Regosols"]
ggplot(data = hru.mnt.hr,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Regosols in Mountaneous HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.mnt.hr, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Regosols in Mountaneous HRU level")
ggplot(data = hru.mnt.hr, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Regosols in Mountaneous HRU level")


hru.mnt.hfa <- hru.mnt.avr[ SOIL == "Hydragric ferralic Anthrosols"]
ggplot(data = hru.mnt.hfa,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Hydragric ferralic Anthrosols in Mountaneous HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.mnt.hfa, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Hydragric ferralic Anthrosols in Mountaneous HRU level")
ggplot(data = hru.mnt.hfa, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Hydragric ferralic Anthrosols in Mountaneous HRU level")


hru.mnt.lr <- hru.mnt.avr[ SOIL == "Leptic Regosols"]
ggplot(data = hru.mnt.lr,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Leptic Regosols in Mountaneous HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.mnt.lr, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Leptic Regosols in Mountaneous HRU level")
ggplot(data = hru.mnt.lr, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Leptic Regosols in Mountaneous HRU level")



hru.lss.ck <- hru.lss.avr[ SOIL == "Calcic Kastanozems"]
ggplot(data = hru.lss.ck,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Kastanozems in Loess HRU level") + ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.lss.ck, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Kastanozems in Loess HRU level")
ggplot(data = hru.lss.ck, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Kastanozems in Loess HRU level")


hru.lss.cl <- hru.lss.avr[ SOIL == "Calcic Luvisols"]
ggplot(data = hru.lss.cl,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Luvisols in Loess HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.lss.cl, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Luvisols in Loess HRU level")
ggplot(data = hru.lss.cl, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Calcic Luvisols in Loess HRU level")


hru.lss.hc <- hru.lss.avr[ SOIL == "Haplic Calcisols"]
ggplot(data = hru.lss.hc,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Calcisols in Loess HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.lss.hc, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Calcisols in Loess HRU level")
ggplot(data = hru.lss.hc, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Calcisols in Loess HRU level")


hru.lss.hl <- hru.lss.avr[ SOIL == "Haplic Luvisols"]
ggplot(data = hru.lss.hl,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Luvisols in Loess HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.lss.hl, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Luvisols in Loess HRU level")
ggplot(data = hru.lss.hl, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Luvisols in Loess HRU level")


hru.lss.hr <- hru.lss.avr[ SOIL == "Haplic Regosols"]
ggplot(data = hru.lss.hr,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) + geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Regosols in Loess HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.lss.hr, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Regosols in Loess HRU level")
ggplot(data = hru.lss.hr, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Haplic Regosols in Loess HRU level")


hru.lss.lr <- hru.lss.avr[ SOIL == "Leptic Regosols"]
ggplot(data = hru.lss.lr,mapping = aes(x= LANDUSE, y = avg.ET, fill= SLOPE_CD)) +   geom_bar(stat = "identity" ,position = "dodge") + theme_bw() + ylab("mean ET [mm]") +   ggtitle("Yearly mean ET for Leptic Regosols in Loess HRU level") +  ylim(0, 900) + geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9))
ggplot(data = hru.lss.lr, mapping = aes(x= LANDUSE, y = avg.ET, col= LANDUSE)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Leptic Regosols in Loess HRU level")
ggplot(data = hru.lss.lr, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none")+ ylab("mean ET [mm]") + ggtitle("Yearly mean ET for Leptic Regosols in Loess HRU level")



hru.mnt.gr1 <- data.table(rbind(hru.mnt.cc, hru.mnt.ck, hru.mnt.cl))
ggplot(data = hru.mnt.gr1, mapping = aes(x= LANDUSE, y=avg.ET, fill= SLOPE_CD)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(SOIL ~ .)  + ylab("mean ET [mm]") + 
  geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9)) +
  ggtitle("Yearly mean ET in Mountaneous HRU level")


hru.mnt.gr2 <- data.table(rbind(hru.mnt.hf, hru.mnt.hl,hru.mnt.hr))
ggplot(data = hru.mnt.gr2, mapping = aes(x= LANDUSE, y=avg.ET, fill= SLOPE_CD)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(SOIL ~ .)  + ylab("mean ET [mm]") + 
  geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9)) + 
  ggtitle("Yearly mean ET in Mountaneous HRU level")

hru.mnt.gr3 <- data.table(rbind(hru.mnt.fsc, hru.mnt.hfa, hru.mnt.lr))
ggplot(data = hru.mnt.gr3, mapping = aes(x= LANDUSE, y=avg.ET, fill= SLOPE_CD)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(SOIL ~ .)  + ylab("mean ET [mm]") + 
  geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9)) +
  ggtitle("Yearly mean ET in Mountaneous HRU level")

hru.lss.gr1 <- data.table(rbind(hru.lss.ck, hru.lss.cl, hru.lss.lr))
ggplot(data = hru.lss.gr1, mapping = aes(x= LANDUSE, y=avg.ET, fill= SLOPE_CD)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(SOIL ~ .)  + ylab("mean ET [mm]") + 
  geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9)) +
  ggtitle("Yearly mean ET in Loess HRU level")


hru.lss.gr2 <- data.table(rbind(hru.lss.hc, hru.lss.hl, hru.lss.hr))
ggplot(data = hru.lss.gr2, mapping = aes(x= LANDUSE, y=avg.ET, fill= SLOPE_CD)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(SOIL ~ .)  + ylab("mean ET [mm]") + 
  geom_errorbar(aes(ymin= avg.ET -se, ymax= avg.ET+se), width = 0.2, position=position_dodge(0.9)) +
  ggtitle("Yearly mean ET in Loess HRU level")






hru.mnt.corn <- hru.mnt.avr[LANDUSE=="CORN"]
ggplot(data = hru.mnt.corn, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for CORN in Mountain HRU level")
hru.mnt.frsd <- hru.mnt.avr[LANDUSE=="FRSD"]
ggplot(data = hru.mnt.frsd, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for FRSD in Mountain HRU level")
hru.mnt.lari <- hru.mnt.avr[LANDUSE=="LARI"]
ggplot(data = hru.mnt.lari, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for LARI in Mountain HRU level")
hru.mnt.past <- hru.mnt.avr[LANDUSE=="PAST"]
ggplot(data = hru.mnt.past, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for PAST in Mountain HRU level")
hru.mnt.pine <- hru.mnt.avr[LANDUSE=="PINE"]
ggplot(data = hru.mnt.pine, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for PINE in Mountain HRU level")
hru.mnt.wwht <- hru.mnt.avr[LANDUSE=="WWHT"]
ggplot(data = hru.mnt.wwht, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for WWHT in Mountain HRU level")



hru.lss.corn <- hru.lss.avr[LANDUSE=="CORN"]
ggplot(data = hru.lss.corn, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for CORN in Loess HRU level")
hru.lss.frsd <- hru.lss.avr[LANDUSE=="FRSD"]
ggplot(data = hru.lss.frsd, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for FRSD in Loess HRU level")
hru.lss.lari <- hru.lss.avr[LANDUSE=="LARI"]
ggplot(data = hru.lss.lari, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for LARI in Loess HRU level")
hru.lss.past <- hru.lss.avr[LANDUSE=="PAST"]
ggplot(data = hru.lss.past, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for PAST in Loess HRU level")
hru.lss.pine <- hru.lss.avr[LANDUSE=="PINE"]
ggplot(data = hru.lss.pine, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for PINE in Loess HRU level")
hru.lss.wwht <- hru.lss.avr[LANDUSE=="WWHT"]
ggplot(data = hru.lss.wwht, mapping = aes(x= SLOPE_CD, y = avg.ET, col= SLOPE_CD)) + geom_boxplot(position = "dodge", varwidth = TRUE) + theme_bw() + theme(legend.position="none") + ylab("mean ET [mm]") + ggtitle("Yearly mean ET for WWHT in Loess HRU level")
