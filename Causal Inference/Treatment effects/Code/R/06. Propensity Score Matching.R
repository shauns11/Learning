.libPaths()
print(R.version.string)

Macdrive<-"/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive<-"C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder<-"Causal Inference/Treatment Effects"  
setwd(paste0(UCLdrive, folder))
#setwd(paste0(Macdrive, folder))
library(haven)
library(glue)
library(MatchIt)

url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
df <-haven::read_dta(glue(url_stem,'Data/cattaneo2.dta'))
head(df)
    

##################################################
#Example 1: 
#matching variables:mmarried; mage; medu; fbaby
#distance= mahalanobis
#(One-to-one).
###################################################


#The match.data() output is preferred when pair membership is not 
#directly included in the analysis; 
    
#The get_matches() output is preferred when 
#pair membership is to be included.
    
#NN Mahalanobis distance matching w/ replacement.
m.out <- matchit(mbsmoke ~ mmarried + mage + medu + fbaby, 
                     data = df,
                     distance = "mahalanobis", ties=TRUE,
                     replace = TRUE)
m.out
summary(m.out)
    
#1151 obs matched.
#864 Tx matched with 287 control (meaning 864 pairs).
#864*2 = 1728.
#EACH TX=1 has been matched with at least 1 control.
    
#match data.
    
md <- match.data(m.out, data = df,
                     distance = "prop_score")
dim(md) #one row per matched unit [N=1151].
head(md, 10)
    
#get matches
gm <- get_matches(m.out, data = df,
                      distance = "prop_score")
dim(gm) #multiple rows per matched unit
head(gm, 10)
#864 pairs (1728 rows)
    
#Number of control units in each match stratum
table(table(gm$subclass[gm$A == 0]))
table(table(gm$subclass[gm$A == 1]))
    
#match.data() output
fit1md <- lm(bweight ~ mbsmoke, data = md, weights = weights)
fit1md
    

##################################################
#Example 2: 
#matching variables:mmarried; mage; medu; fbaby
#distance= mahalanobis 
#ratio=2 (2 controls for each Tx)
###################################################


# NN Mahalanobis distance matching w/ replacement.
m.out <- matchit(mbsmoke ~ mmarried + mage + medu + fbaby, 
                     data = df,
                     distance = "mahalanobis", ties=TRUE,ratio=2,
                     replace = TRUE)
m.out
summary(m.out)
    
#1409 obs matched.
#864 Tx matched with 545 control (meaning 864 pairs).
#864*2 = 1728.
#EACH TX=1 has been matched with at least 1 control.
    
#match data.
md <- match.data(m.out, data = df,
                     distance = "prop_score")
dim(md) #one row per matched unit [N=1409].
head(md, 10)

#get matches
    
gm <- get_matches(m.out, data = df,
                      distance = "prop_score")
dim(gm) #multiple rows per matched unit
head(gm, 10)

#864 pairs (2592 rows)
#View(gm)
#(1 Tx + 1 control + 1 control) * 864.
#864*3 = 2592.
    
#Number of control units in each match stratum
table(table(gm$subclass[gm$mbsmoke == 0])) # 2 control for each Tx.
    
#match.data() output
fit1md <- lm(bweight ~ mbsmoke, data = md, weights = weights)
fit1md
    
##################################################
#Example 3: 
#matching variables:mmarried; mage; medu; fbaby
#distance= NNM 
#ratio=1 
###################################################

#Nearest neighbour
m.out <- matchit(mbsmoke ~ mmarried + mage + medu + fbaby, 
                     data = df,
                     method = "nearest", ties=TRUE,ratio=1,
                     replace = TRUE)
m.out
summary(m.out)
    
#1155 obs matched.
#864 Tx matched with 291 control (meaning 864 pairs).
#864*2 = 1728.
#EACH TX=1 has been matched with at least 1 control.
    
#match data.
md <- match.data(m.out, data = df,
                     distance = "prop_score")
dim(md) #one row per matched unit [N=1409].
head(md, 10)
    
#get matches
gm <- get_matches(m.out, data = df,
                      distance = "prop_score")
dim(gm) #multiple rows per matched unit
head(gm, 10)
#864 pairs (1728 rows)
    
#Number of control units in each match stratum
table(table(gm$subclass[gm$mbsmoke == 0])) # 1 control for each Tx.
    
#match.data() output
fit1md <- lm(bweight ~ mbsmoke, data = md, weights = weights)
fit1md


##################################################
#Example 4: 
#matching variables:mmarried; mage; medu; fbaby
#distance= NNM 
#variable number of matches 
###################################################


m.out <- matchit(mbsmoke ~ mmarried + mage + medu + fbaby, 
                     data = df, 
                     ties=TRUE,
                     replace = TRUE,
                     method = "nearest",
                     ratio = 2,
                     min.controls = 1, max.controls = 80)
m.out
summary(m.out)
    
#1462 obs matched.
#864 Tx matched with 598 control (meaning 864 pairs).
#864*2 = 1728.
#EACH TX=1 has been matched with at least 1 control.
    
#match data.
md <- match.data(m.out, data = df,
                     distance = "prop_score")
dim(md) #one row per matched unit [N=1151].
head(md, 10)
    
#get matches
gm <- get_matches(m.out, data = df,
                      distance = "prop_score")
dim(gm) #multiple rows per matched unit
head(gm, 10)
#864 pairs (1728 rows)
    
#Number of control units in each match stratum
table(table(gm$subclass[gm$mbsmoke == 0]))
    
#match.data() output
fit1md <- lm(bweight ~ mbsmoke, data = md, weights = weights)
fit1md

print(paste("06 PSM matching finished at", format(Sys.time(), 
                              "%Y-%m-%d %H:%M:%S")))

##########
#FINISHED
##########
    
    
    