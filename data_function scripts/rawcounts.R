##### find raw co-occ counts #####

rawcounts <- function(tokens.pos) {
  #ensure Matrix (SparseM} or matrix {base} format
  require(Matrix)
  
  minimumFrequency = 10
  binDTM <- tokens.pos %>% 
    dfm() %>% 
    dfm_trim(min_docfreq = minimumFrequency) %>% 
    dfm_weight("boolean")
  
  #ensure binary DTM
  if (any(binDTM > 1)) {
    binDTM[binDTM > 1] <- 1
  }
  
  #calculate co-occurrence counts
  coocCounts <- t(binDTM) %*% binDTM
  return(coocCounts)
}
