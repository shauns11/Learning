#https://ardata-fr.github.io/flextable-book/index.html

rm()
library(dplyr)
library(haven)
library(survey)
library(flextable)
library(officer)
library(tibble)
library(dplyr)
library(gtable)
library(summarytools)

set.seed(12345)

#--------------------------------------------------
# 1. Create synthetic survey data
#--------------------------------------------------

df <- tibble(id = 1:1000) %>%
  mutate(
    sex = as.integer(runif(n()) > 0.5),
    height = rnorm(n(), 1.65 + 0.10 * sex, 0.08),
    weight = rnorm(n(), 65 + 10 * sex, 12),
    bmi = weight / height^2,
    psu = ceiling(id/10),
    strata = (id %% 5) + 1,
    weightp = runif(n()) * 2 + 0.5,
    age = floor(runif(n()) * 41) + 50,
    wealth = ceiling(runif(n()) * 5)
  )

df$sex <- factor(df$sex, levels = c(0,1), 
                 labels = c("Male","Female"))
table(df$sex)

df <- df %>% 
  mutate(agegroup = dplyr::case_when(
    age >=10 & age <55 ~ "50-54",
    age >=55 & age <60 ~ "55-59",
    age >=60 & age <65 ~ "60-64",
    age >=65 & age <70 ~ "65-69",
    age >=70 & age <75 ~ "70-74",
    age >=75 & age <80 ~ "75-79",
    age >=80  ~ "80+"))
summarytools::freq(df$agegroup,cumul=FALSE)


df <- df %>% 
  mutate(obese = dplyr::case_when(
    bmi >=0 & bmi <30 ~ 0,
    bmi >=30 ~ 1))
summarytools::freq(df$obese,cumul=FALSE)




saveRDS(df,"C:/R/Data management/Data/survey_data.rds")

#load
df <- readRDS("C:/R/Data management/Data/survey_data.rds")
head(df)

svy_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weightp,
  nest = TRUE,
  data = df)
svy_design

t1a <- survey::svyby(
  ~bmi,
  ~sex,
  svy_design,
  svymean,
  na.rm = TRUE
)

t1b <- survey::svyby(
  ~bmi,
  ~sex,
  svy_design,
  unwtd.count,
  na.rm = TRUE
)

# 3. Merge results
t1c <- merge(t1a, t1b, by = c("sex"))
t1c
cols <- c("sex", "bmi", "counts")
t1c <- t1c[, cols]


t2a <- survey::svyby(
  ~bmi,
  ~sex+agegroup,
  svy_design,
  svymean,
  na.rm = TRUE
)
# Sort by sex, then agegroup
t2a<- t2a[order(t2a$sex, t2a$agegroup), ]

t2b <- survey::svyby(
  ~bmi,
  ~sex+agegroup,
  svy_design,
  unwtd.count,
  na.rm = TRUE
)
# Sort by sex, then agegroup
t2b<- t2b[order(t2b$sex, t2b$agegroup), ]

# 3. Merge results
t2c <- merge(t2a, t2b, by = c("sex","agegroup"))
t2c
cols <- c("sex", "agegroup", "bmi", "counts")
t2c <- t2c[, cols]

men_bmi<-c(t2c[8:14,3],t1c[2,2])
women_bmi<-c(t2c[1:7,3],t1c[1,2])
men_bmi_counts<-c(t2c[8:14,4],t1c[2,3])
women_bmi_counts<-c(t2c[1:7,4],t1c[1,3])
men_bmi
women_bmi
men_bmi_counts
women_bmi_counts

men_bmi <- round(men_bmi, 1)
women_bmi <- round(women_bmi, 1)



df <- data.frame(rbind(men_bmi, women_bmi, men_bmi_counts, women_bmi_counts))
df
rownames(df) <- c("Men", "Women", "Men N", "Women N")
df
colnames(df) <- c("50-54", "55-59", "60-64", "65-69",
                  "70-74", "75-79", "80+", "All")
library(tibble)
library(flextable)

df <- rownames_to_column(df, var = "Group")
ft <- flextable(df)
ft

ft <- colformat_num(
  ft,
  i = 3:4,      # rows
  j = 2:9,  # numeric columns (adjust if needed)
  digits = 0
)
ft

doc <- read_docx() %>%
  body_add_par("Table 1: Mean BMI by Sex", 
style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "C:/R/Officer/outputs/Table1_BMI_by_sex.docx")

??colformat_num


###############################
##############################


??flextable






