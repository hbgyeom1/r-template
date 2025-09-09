library(survival); library(data.table); library(jstable)
setwd("~/Documents/code/r-template")

a <- as.data.table(colon)

varlist <- list(
  Strata = "rx",
  Base = c("age", "sex", "surg", "extent")
)

out <- a[, .SD, .SDcols = c(unlist(varlist))]

factor_vars <- c(names(out)[sapply(out, function(x){length(table(x))}) <= 10])
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
conti_vars <- setdiff(names(out), c(factor_vars))
out[, (conti_vars) := lapply(.SD, as.numeric), .SDcols = conti_vars]

out.label <- jstable::mk.lev(out)

vars01 <- sapply(factor_vars, function(v){
  identical(levels(out[[v]]), c("0", "1"))})
for (v in names(vars01)[vars01 == T]){
  out.label[variable == v, val_label := c("No", "Yes")]
}

out.label[variable == "age", var_label := "Age - median (range)"]
out.label[variable == "sex", `:=` (var_label = "Sex - male",
                                   val_label = c("Female", "Male"))]
out.label[variable == "surg", `:=` (var_label = "Days since surgery",
                                    val_label = c("7-20", "21-35"))]
out.label[variable == "extent", `:=` (var_label = "Depth of invasion",
                                    val_label = c("1", "2", "3", "4"))]

tb1 <- CreateTableOneJS(vars = varlist$Base, strata = varlist$Strata, data = a,
                        factorVars = factor_vars, labeldata = out.label, Labels = T)$table

tb1