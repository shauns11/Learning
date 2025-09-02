.libPaths()
print(R.version.string)

Macdrive <- "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive <-"C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder <- "Causal Inference/Treatment Effects"  
setwd(paste0(UCLdrive, folder))
#setwd(paste0(Macdrive, folder))


library(MatchIt)
library(glue)
library(haven)

url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
data <-haven::read_dta(glue(url_stem,'Data/cattaneo2.dta'))
head(data)

#One-to-one matching
m.out <- MatchIt::matchit(mbsmoke ~ mmarried + mage,
                          distance = "mahalanobis",
                          method = "nearest",
                          estimand = "ATT",
                          ratio = 1, replace=TRUE,
                          data = data)
m.out
summary(m.out)
md<-match.data(m.out)
fit1 <- lm(bweight ~ mbsmoke, data = md,weights = weights)
print(fit1)

#Example 2.
url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
data <-haven::read_dta(glue(url_stem,'Data/cattaneo2.dta'))
head(data)

#One-to-one matching
m.out <- MatchIt::matchit(mbsmoke ~ mmarried + mage + fage + medu + prenatal1,
                               exact ~ mmarried + prenatal1,
                               distance = "mahalanobis",
                               method = "nearest",
                               estimand = "ATT",
                               ratio = 1,
                               data = data)
m.out
summary(m.out)
md<-match.data(m.out)
fit1 <- lm(bweight ~ mbsmoke, data = md,weights = weights)
print(fit1)

print(paste("05 NNM finished at", format(Sys.time(), 
                          "%Y-%m-%d %H:%M:%S")))
##########.
#FINISHED.
##########.

    
    
    
    