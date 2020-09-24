# Matches municipality codes to IATA codes of Norwegian airports

# Import air travel statistics
df <- read.csv("air-travel-raw.csv")
df$k <- df$IATA

df$k <- ifelse(df$k=="ALF",5403,
        ifelse(df$k=="ANX",1871,
        ifelse(df$k=="BDU",5418,
        ifelse(df$k=="BGO",4601,
        ifelse(df$k=="BVG",5440,
        ifelse(df$k=="BOO",1804,
        ifelse(df$k=="BNN",1813,
        ifelse(df$k=="BJF",5443,
        ifelse(df$k=="FRO",4602,
        ifelse(df$k=="FDE",4647,
        ifelse(df$k=="HFT",5406,
        ifelse(df$k=="EVE",5402,
        ifelse(df$k=="HAA",5433,
        ifelse(df$k=="HAU",1106,
        ifelse(df$k=="HVG",5435,
        ifelse(df$k=="KKN",5444,
        ifelse(df$k=="KRS",4204,
        ifelse(df$k=="KSU",1505,
        ifelse(df$k=="LKL",5436,
        ifelse(df$k=="LKN",1860,
        ifelse(df$k=="MEH",5439,
        ifelse(df$k=="MQN",1833,
        ifelse(df$k=="MOL",1506,
        ifelse(df$k=="MJF",1824,
        ifelse(df$k=="OSL",301,
        ifelse(df$k=="OSY",5007,
        ifelse(df$k=="RRS",5025,
        ifelse(df$k=="RVK",5060,
        ifelse(df$k=="RET",1856,
        ifelse(df$k=="SDN",4650,
        ifelse(df$k=="SSJ",1820,
        ifelse(df$k=="SOG",4640,
        ifelse(df$k=="SVG",1103,
        ifelse(df$k=="SKN",1866,
        ifelse(df$k=="LYR",NA,
        ifelse(df$k=="SVJ",1865,
        ifelse(df$k=="SOJ",5428,
        ifelse(df$k=="TOS",5401,
        ifelse(df$k=="TRD",5001,
        ifelse(df$k=="VDS",5405,
        ifelse(df$k=="VAW",5404,
        ifelse(df$k=="VRY",1857,
        ifelse(df$k=="HOV",NA,
        ifelse(df$k=="AES",1507,9999))))))))))))))))))))))))))))))))))))))))))))

df <- df[,-c(1,2,3,4,5)]
colnames(df) <- c("passasjererutland","kommunenummer")
write.csv(df,"air-travel-modified.csv")


