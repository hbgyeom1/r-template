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


## Run
library(mgcv)
dlist <- list(All = out, Exclude_lumbar_qc_bad = out[lumbar_qc_bad == 0])

reslist <- lapply(dlist, function(dd){
  ## tb1
  tb1 <- CreateTableOneJS(vars = setdiff(varlist$Base, "DM"), strata = "DM",
                          data = dd, labeldata = out.label,
                          Labels = T, showAllLevels = F)$table
  
  ## lm
  res.linear <- lapply(c("Femur", "L1.L4"), function(y) {
    m1 <- glm(as.formula(paste(y, "~ DM + Age + BMI")), data = dd)
    m2 <- glm(as.formula(paste(y, "~ DM + Age")), data = dd)
    m0 <- glm(as.formula(paste(y, "~ DM")), data = dd)
    res <- LabeljsTable(glmshow.display(m1, decimal = 2)$table, ref = out.label)
    res[, 1] <- c(res[1, 1], "", "")
    res[, 2] <- c(res[1, 2], "", "")
    res2 <- LabeljsTable(glmshow.display(m2, decimal = 2)$table, ref = out.label)[, 3:4] %>% 
      rbind(BMI = c("", ""))
    info.rsq <- lapply(list(m0, m2, m1), \(x){c(paste0("R^2 = ", round(performance::r2(x)$R2, 3)), "")}) %>% unlist
    
    
    res.final <- cbind(res[, 1:2], res2, res[, 3:4]) %>% 
      rbind(info.rsq, .)
    colnames(res.final)[-c(1:2)] <- c("Age adjusted coeff.(95%CI)", "Age adjusted p", "Age & BMI adjusted coeff.(95%CI)", "Age & BMI adjusted p")
    rownames(res.final)[1] <- paste0(y, " (n = ", nrow(dd[!is.na(get(y))]), ")")
    return(res.final)
  }) %>% do.call(rbind, .)
  
  ## Logistic
  res.logistic <- lapply(c("osteoporosis"), function(y) {
    m1 <- glm(as.formula(paste(y, "~ DM + Age + BMI")), data = dd, family = binomial)
    m2 <- glm(as.formula(paste(y, "~ DM + Age")), data = dd, family = binomial)
    m0 <- glm(as.formula(paste(y, "~ DM")), data = dd, family = binomial)
    res <- LabeljsTable(glmshow.display(m1, decimal = 2)$table, ref = out.label)
    res[, 1] <- c(res[1, 1], "", "")
    res[, 2] <- c(res[1, 2], "", "")
    res2 <- LabeljsTable(glmshow.display(m2, decimal = 2)$table, ref = out.label)[, 3:4] %>% 
      rbind(BMI = c("", ""))
    
    info.auc <- lapply(list(m0, m2, m1), \(x){
      obj.roc <- pROC::roc(as.numeric(x$model[[1]]), x$fitted.values, direction = "<")
      ci <- pROC::ci.auc(obj.roc)
      c(paste0("AUC = ", round(obj.roc$auc, 3), " (", round(ci[1], 3), "-", round(ci[3], 3), ")"), "")
    }) %>% unlist
    
    res.final <- cbind(res[, 1:2], res2, res[, 3:4]) %>% 
      rbind(info.auc, .)
    colnames(res.final)[-c(1:2)] <- c("Age adjusted OR(95%CI)", "Age adjusted p", "Age & BMI adjusted OR(95%CI)", "Age & BMI adjusted p")
    rownames(res.final)[1] <- paste0(y, " (n = ", nrow(dd[!is.na(get(y))]), ")")
    return(res.final)
  }) %>% do.call(rbind, .)
  
  ## GAM: HbA1c
  res.gam <- lapply(c("Femur", "L1.L4"), function(y) {
    # Fit models in a list
    models <- list(
      m0 = gam(as.formula(paste(y, "~ s(HbA1c)")), data = dd),
      m2 = gam(as.formula(paste(y, "~ s(HbA1c) + Age")), data = dd),
      m1 = gam(as.formula(paste(y, "~ s(HbA1c) + Age + BMI")), data = dd)
    )
    
    ## Non-linearity test: Effective DF
    info.nl <- lapply(models, \(x){summary(x)$s.table}) %>% do.call(rbind, .)
    rownames(info.nl) <- c("Unadjusted", "Age adjusted", "Age & BMI adjusted")
    
    # Create HbA1c range
    hba1c_range <- seq(min(dd$HbA1c, na.rm = T), max(dd$HbA1c, na.rm = T), length.out = 100)
    
    # Extract smooth terms for all models using lapply
    smooth_results <- lapply(models, function(m) {
      # Create prediction data frame based on model terms
      pred_data <- data.frame(HbA1c = hba1c_range)
      if("Age" %in% all.vars(formula(m))) pred_data$Age <- mean(dd$Age, na.rm = T)
      if("BMI" %in% all.vars(formula(m))) pred_data$BMI <- mean(dd$BMI, na.rm = T)
      
      # Get predictions
      pred_terms <- predict(m, newdata = pred_data, type = "terms", se.fit = T)
      list(
        smooth = pred_terms$fit[, grep("s\\(HbA1c\\)", colnames(pred_terms$fit))],
        se = pred_terms$se.fit[, grep("s\\(HbA1c\\)", colnames(pred_terms$se.fit))]
      )
    })
    
    # Find reference value at HbA1c = 6.5 for centering
    ref_idx <- which.min(abs(hba1c_range - 6.5))
    refs <- sapply(smooth_results, function(x) x$smooth[ref_idx])
    
    # Create model labels with edf and p-values
    model_names <- c("Unadjusted", "Age adjusted", "Age & BMI adjusted")
    model_labels <- sapply(1:3, function(i) {
      paste0(model_names[i], " (edf = ", round(info.nl[i, "edf"], 2), 
             ", p ", ifelse(info.nl[i, 4] < 0.001, "< 0.001", 
                            paste0("= ", round(info.nl[i, 4], 3))), ")")
    })
    
    # Create plot data - combine all results
    plot_data <- data.frame(
      HbA1c = rep(hba1c_range, 3),
      fit = unlist(lapply(1:3, function(i) smooth_results[[i]]$smooth - refs[i])),
      se = unlist(lapply(smooth_results, function(x) x$se)),
      Model = factor(rep(model_labels, each = 100), levels = model_labels)
    )
    
    # Calculate confidence intervals
    plot_data$lower <- plot_data$fit - 1.96 * plot_data$se
    plot_data$upper <- plot_data$fit + 1.96 * plot_data$se
    
    # Create ggplot
    library(ggplot2)
    p <- ggplot(plot_data, aes(x = HbA1c, y = fit, color = Model, fill = Model)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
      geom_line(size = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = 6.5, linetype = "dotted", color = "gray50") +
      labs(x = "HbA1c (%)",
           y = paste0("Smooth effect on ", y, " (centered at HbA1c = 6.5)"),
           title = paste0("Non-linear association between HbA1c and ", y, " (n = ", nrow(models$m0$model), ")"),
           color = "Model",
           fill = "Model") +
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank()) +
      scale_color_manual(values = structure(c("#4DAF4A", "#377EB8", "#E41A1C"), 
                                            names = model_labels)) +
      scale_fill_manual(values = structure(c("#4DAF4A", "#377EB8", "#E41A1C"), 
                                           names = model_labels))
    
    
    # Also return the plot object
    return(p)
  })
  names(res.gam) <- c("Femur", "L1.L4")
  
  return(list(Table1 = tb1, Linear = res.linear, Logistic = res.logistic, GAM = res.gam))
})

#reslist

## Save results to Excel and PPT
library(openxlsx);library(officer);library(rvg)

# Save Excel files for each dataset
for(nm in names(reslist)){
  wb <- createWorkbook()
  
  # Add Table1 sheet
  addWorksheet(wb, "Table1")
  writeData(wb, sheet = "Table1", reslist[[nm]]$Table1, rowNames = T)
  
  # Add Linear regression sheet
  addWorksheet(wb, "Linear")
  writeData(wb, sheet = "Linear", reslist[[nm]]$Linear, rowNames = T)
  
  # Add Logistic regression sheet
  addWorksheet(wb, "Logistic")
  writeData(wb, sheet = "Logistic", reslist[[nm]]$Logistic, rowNames = T)
  
  # Save Excel file
  saveWorkbook(wb, file = paste0("results_", nm, ".xlsx"), overwrite = T)
}

# Save PPT files for each dataset with GAM plots
for(nm in names(reslist)){
  ppt <- read_pptx()
  
  # Add slides for each GAM plot (Femur and L1.L4)
  for(outcome in names(reslist[[nm]]$GAM)){
    ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
    ppt <- ph_with(ppt, 
                   dml(ggobj = reslist[[nm]]$GAM[[outcome]]), 
                   location = ph_location(left = 0, top = 0, width = 10, height = 7.5))
  }
  
  # Save PPT file
  print(ppt, target = paste0("GAM_plots_", nm, ".pptx"))
}


