change_cols_names <- function(data, new_prefix_to_add, remove_chars=T){
  idx_cols_to_be_renamed <- grepl("^[[:digit:]]", colnames(data))
  if(remove_chars){
    col_names <- sapply(colnames(data)[idx_cols_to_be_renamed], 
                        function(x) gsub('*_T', '', x))
  }else col_names <- colnames(data)[idx_cols_to_be_renamed]
  new_names <- paste0(new_prefix_to_add, col_names)
  colnames(data)[idx_cols_to_be_renamed] <- new_names
  return(data)
}

calculate_ratio <- function(num, den){
  ratio <- num / den * 1000 # from tons to kg
  return(ratio)
}

calculate_ratio_cols <- function(data){
  years_as_chars <- (1996:2020) %>% as.character()
  col_names <- colnames(data)
  idx_co2_col <- 7
  idx_km_col <- 32
  new_cols <- rep(NA, nrow(data))
  for(i in idx_co2_col:(idx_km_col-1)){
    num <- data %>% select(all_of(col_names[idx_co2_col]))
    den <- data %>% select(all_of(col_names[idx_km_col]))
    ratio <- calculate_ratio(num, den)
    new_cols <- cbind(new_cols, ratio)
    idx_co2_col <- idx_co2_col+1
    idx_km_col <- idx_km_col+1
  }
  new_cols <- new_cols[,-1]
  colnames(new_cols) <- paste('ratio_CO2_km_', years_as_chars, sep='')
  data <- cbind(data, new_cols)
  return(data)
}

weighted_avg_col <- function(data){
  data_tmp <- data[,7:ncol(data)]
  idx_km_col <- 26
  idx_ratio_col <- 51
  weighted_emission <- NULL
  for(i in 0:(2020-1996)){
    km <- data_tmp[,i+idx_km_col]
    ratio <- data_tmp[,i+idx_ratio_col]
    ratio[which.max(km)]
    weighted_emission <- c(weighted_emission,
                           weighted.mean(ratio, km) )
  }
  weighted_emission <- data.frame(year = 1996:2020,
                                  weighted_emission = weighted_emission)
  return(weighted_emission)
}
