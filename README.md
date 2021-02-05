# forestplot
```
rm(list = ls())
load("cox_dat.Rdata")
str(dat)
library(survival)
library(survminer)
library(forestplot)
library(stringr)
#cox回归，建立模型
model <- coxph(Surv(time, event) ~., data = dat )
ggforest(model)
#summary
m = summary(model)
colnames(m$coefficients)
#[1] "coef"      "exp(coef)" "se(coef)"  "z"         "Pr(>|z|)" 
colnames(m$conf.int)
#[1] "exp(coef)"  "exp(-coef)" "lower .95"  "upper .95"
#p值改一下格式，加上显著性
p = ifelse(
  m$coefficients[, 5] < 0.001,
  "<0.001 ***",
  ifelse(
    m$coefficients[, 5] < 0.01,
    "<0.01  **",
    ifelse(
      m$coefficients[, 5] < 0.05,
      paste(round(m$coefficients[, 5], 3), " *"),
      round(m$coefficients[, 5], 3)
    )
  )
)
p
#HR和它的置信区间
dat2 = as.data.frame(round(m$conf.int[, c(1, 3, 4)], 2))
dat2 = tibble::rownames_to_column(dat2, var = "Trait")
colnames(dat2)[2:4] = c("HR", "lower", "upper")
#需要在图上显示的HR文字和p值
dat2$HR2 = paste0(dat2[, 2], "(", dat2[, 3], "-", dat2[, 4], ")")
dat2$p = p
str(dat2)

#'data.frame':	10 obs. of  6 variables:
#$ Trait: chr  "gendermale" "stageii" "stageiii" "stageiv" ...
#$ HR   : num  1.08 1.22 2.5 6.39 1.03 1.41 0.86 0.89 0.82 0.95
#$ lower: num  0.76 0.61 1.6 4.12 1.01 1.14 0.74 0.75 0.76 0.85
#$ upper: num  1.53 2.46 3.89 9.9 1.05 1.75 0.99 1.05 0.9 1.06
#$ HR2  : chr  "1.08(0.76-1.53)" "1.22(0.61-2.46)" "2.5(1.6-3.89)" "6.39(4.12-9.9)" ...
#$ p    : chr  "0.668" "0.574" "<0.001 ***" "<0.001 ***" ...

#基础画图 
forestplot(
  dat2[, c(1, 4, 6)],
  mean = dat2[, 2],
  lower = dat2[, 3],
  upper = dat2[, 4],
  zero = 1,
  boxsize = 0.4,
  col = fpColors(box = '#1075BB', lines = 'black', zero = 'grey'),
  lty.ci = "solid",
  graph.pos = 2
)

# - -----------------------------------------------------------------------
#修饰
dat2$Trait = str_remove(dat2$Trait, "gender|stage")

ins = function(x) {
  c(x, rep(NA, ncol(dat2) - 1))
}
#重点是矩阵如何建立
dat2 = rbind(
  c("Trait", NA, NA, NA, "HR", "p"),
  ins("gender"),
  ins("female"),
  dat2[1, ],
  ins("stage"),
  ins("i"),
  dat2[2:nrow(dat2), ]
)
for(i in 2:4) {
  dat2[, i] = as.numeric(dat2[, i])
}
str(dat2)
forestplot(
  dat2[, c(1, 5, 6)],
  mean = dat2[, 2],
  lower = dat2[, 3],
  upper = dat2[, 4],
  zero = 1,
  boxsize = 0.4,
  col = fpColors(box = '#1075BB', lines = 'black', zero = 'grey'),
  lty.ci = "solid",
  graph.pos = 2,
  #xticks = F,
  is.summary = c(T, T, F, F, T, rep(F, 10)),
  align = "l",
  hrzl_lines = list(
    "1" = gpar(lty=1),
    "2" = gpar(lty=1),
    "16"= gpar(lty=1)),
  colgap = unit(5, 'mm')
)

```
