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
library(surveytable)

# Import Stata file
data <- read_dta("D:/survey_data.dta")
head(data)

class(data$bmivg52)
data <- data %>%
  mutate(
    bmivg52 = factor(
      bmivg52,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "underweight",
        "normal",
        "overweight",
        "obese, excl morbidly obese",
        "morbidly obese"
      )
    )
  )

data <- data %>%
  mutate(
    sex.f = factor(
      sex,
      levels = c(0,1),
      labels = c(
        "men",
        "women"
      )
    )
  )


data <- data %>%
  mutate(
    agegrp.f = factor(
      agegrp,
      levels = c(1, 2, 3, 4, 5, 6, 7),
      labels = c(
        "50-54",
        "55-59", "60-64", "65-69", "70-74", "75-79", "80+"
      )
    )
  )

#set missing to -2.
data <- data %>%
  mutate(across(c(bmi,overwt,obese), ~ na_if(., -2)))

svy_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weightp,
  nest = TRUE,
  data = data)
svy_design

#####################
#BMI by sex
#####################
a1 <- survey::svyby(
  ~bmi,
  ~sex,
  svy_design,
  svymean,
  na.rm = TRUE
)

a2 <- survey::svyby(
  ~bmi,
  ~sex,
  svy_design,
  unwtd.count,
  na.rm = TRUE
)

a3<-svytable(~sex, svy_design)
a3

#####################
#bmi by sex and age
#####################
b1 <- survey::svyby(
  ~bmi,
  ~sex+agegrp,
  svy_design,
  svymean,
  na.rm = TRUE
)
b2 <- survey::svyby(
  ~bmi,
  ~sex+agegrp,
  svy_design,
  unwtd.count,
  na.rm = TRUE
)
b3<-svytable(~sex+agegrp, svy_design)
b3

###########
#BMI categories
###########

set_survey(svy_design)
c<-surveytable::tab_subset("bmivg52","sex.f",drop_na=TRUE)
c1 = as.data.frame(c[1])
c2 = as.data.frame(c[2])

men = survey_subset(svy_design,sex==0,"men")
women = survey_subset(svy_design,sex==1,"women")

set_survey(men)
c3<-surveytable::tab_subset("bmivg52","agegrp.f",drop_na=TRUE)
c3 <- do.call(cbind, lapply(c3, function(x) x[1:5, 7, drop = FALSE]))

set_survey(women)
c4<-surveytable::tab_subset("bmivg52","agegrp.f",drop_na=TRUE)
c4 <- do.call(cbind, lapply(c4, function(x) x[1:5, 7, drop = FALSE]))

###############
#overwt and obese
###############

d1 <- survey::svyby(
  ~overwt+obese,
  ~sex,
  svy_design,
  svymean,
  na.rm = TRUE
)

d2 <- survey::svyby(
  ~overwt+obese,
  ~sex+agegrp.f,
  svy_design,
  svymean,
  na.rm = TRUE
)

b1 <- dplyr::arrange(b1, sex, agegrp)
b2 <- dplyr::arrange(b2, sex, agegrp)
d2 <- dplyr::arrange(d2, sex, agegrp.f)

m1<-c(b1[1:7,3],a1[1,2])
m2<-c(b1[1:7,4],a1[1,3])
m3<-c3
m4<-t(c1[1:5,7])
m4<-as.data.frame(t(m4))
m5<-cbind(m3,m4)
m6<-c(t(d2[1:7,3]),d1[1,2])
m7<-c(t(d2[1:7,4]),d1[1,3])

f1<-c(b1[8:14,3],a1[2,2])
f2<-c(b1[8:14,4],a1[2,3])
f3<-c4
f4<-t(c2[1:5,7])
f4<-as.data.frame(t(f4))
f5<-cbind(f3,f4)
f6<-c(t(d2[8:14,3]),d1[2,2])
f7<-c(t(d2[8:14,4]),d1[2,3])

base1<-c(t(b2[1:7,3]),a2[1,2])
base2<-c(t(b2[8:14,3]),a2[2,2])


a3<-as.data.frame(a3)
base3<-c(b3[1,1:7],a3[1,2])
base4<-c(b3[2,1:7],a3[2,2])

#Put these together

t1 = as.data.frame(rbind(m1,m2,m5,m6,m7,
                         f1,f2,f5,f6,f7,
                         base1,base2,base3,base4))
t1

t1[1,1:8]<- round(t1[1,1:8], 1)
t1[2,1:8]<- round(t1[2,1:8], 2)
t1[3:9,1:8]<- round(t1[3:9,1:8], 0)
t1[10,1:8]<- round(t1[10,1:8], 1)
t1[11,1:8]<- round(t1[11,1:8], 2)
t1[12:22,1:8]<- round(t1[12:22,1:8], 0)
t1
colnames(t1) <- c("50-54", "55-59", "60-64", "65-69",
                  "70-74", "75-79", "80+", "All")
t1

library(tibble)
library(flextable)

t1 <- rownames_to_column(t1, var = "Group")
t1[1,1]<-"Mean BMI"
t1[2,1]<-"SE"
t1[3,1]<-"% underweight"
t1[4,1]<-"% normal"
t1[5,1]<-"% overweight"
t1[6,1]<-"% obese, excluding morbidly obese"
t1[7,1]<-"% morbidly obese"
t1[8,1]<-"% Overweight, including obesity"
t1[9,1]<-"% Obese"
t1[10,1]<-"Mean BMI"
t1[11,1]<-"SE"
t1[12,1]<-"% underweight"
t1[13,1]<-"% normal"
t1[14,1]<-"% overweight"
t1[15,1]<-"% obese, excluding morbidly obese"
t1[16,1]<-"% morbidly obese"
t1[17,1]<-"% Overweight, including obesity"
t1[18,1]<-"% Obese"
t1[19,1]<-"Men"
t1[20,1]<-"Women"
t1[21,1]<-"Men"
t1[22,1]<-"Women"
t1


# Create a blank row with same structure
blank_row <- t1[1,]
blank_row[] <- ""
blank_row

# Insert after row 10
t1 <- rbind(
  blank_row,
  t1[1:9, ],
  blank_row,
  t1[10:18, ],
  blank_row,
  t1[19:20, ],
  blank_row,
  t1[21:22, ]
  )
t1

ft <- flextable(t1)
ft

#remove "Group"
ft <- compose(
  ft,
  i = 1, j = 1,
  part = "header",
  value = as_paragraph("")
)
ft

#text in that cell.
ft <- compose(
  ft,
  i = 1, j = 1,
  part = "body",
  value = as_paragraph("Men")
)
ft

#text in that cell.
ft <- compose(
  ft,
  i = 11, j = 1,
  part = "body",
  value = as_paragraph("Women")
)
ft

#text in that cell.
ft <- compose(
  ft,
  i = 21, j = 1,
  part = "body",
  value = as_paragraph("Unweighted bases")
)
ft

#text in that cell.
ft <- compose(
  ft,
  i = 24, j = 1,
  part = "body",
  value = as_paragraph("Weighted bases")
)
ft

ft <- align(ft, j = 2:9, align = "right", part = "body")
ft <- align(ft, j = 2:9, align = "right", part = "header")
ft <- width(ft, j = 1, width = 2.15)  # width in inches


ft <- bold(ft, i = 1, j = 1, bold = TRUE)
ft <- bold(ft, i = 11, j = 1, bold = TRUE)
ft <- italic(ft, i = 21, j = 1, italic = TRUE)
ft <- italic(ft, i = 24, j = 1, italic = TRUE)
ft <- bold(ft, j = 2:9, part = "header", bold=TRUE)

doc <- read_docx() %>%
  body_add_par("Table 1: Mean BMI by Sex", 
               style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "D:/Table1_BMI_by_sex.docx")















    












































