rforest.imp <- function(x, mtry = 2, iter){
  rftmp <- randomForest(as.formula(paste0(x, 
                                          " ~ total.sleep +total.stress + total.exercise + ethnicity + gender + age + class")), 
                      mtry = 2,
                      data = df, 
                      importance = TRUE)
  
  impTmp <- rftmp$importance %>%
    as.data.frame() %>%
    mutate(Variable = rownames(.)) %>%
    gather(measure, value, -Variable) %>%
    mutate(iter = iter)
  
  
  return(impTmp)
}