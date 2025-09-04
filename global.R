library(data.table);library(magrittr);library(jstable)
setwd("~/Documents/code/r-template")



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

out.label[variable == "DM", var_label := "DM_status"]
out.label[variable == "L1.L4", var_label := "L1_4"]

tb1 <- CreateTableOneJS(vars = setdiff(varlist$Base, "DM"), strata = "DM",
                        data = a, labeldata = out.label,
                        Labels = T, showAllLevels = F)$table
