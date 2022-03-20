count_vars_per_gene_per_indiv <- function(data, 
                                        gene_col = 8, 
                                        gt_col_from = 11, 
                                        gt_col_to = ncol(data),
                                        type = c("allele", "position", "carrier"),
                                        ...
                                        ){
  
  if (is.na(type)) stop("Must choose a type from allele, position or carrier") 
  
  count_table <- data %>%
    dplyr::mutate(dplyr::across(.cols = gt_col_from:gt_col_to, 
                                ~ purrr::map_dbl((str_extract_all(., "\\d")), 
                                          ~ sum(as.numeric(.)))))
  
  basic_gene_table <- count_table %>%
    dplyr::group_by(dplyr::across(gene_col)) %>%
    dplyr::summarise(dplyr::across(.cols = names(count_table)[gt_col_from]:names(count_table)[gt_col_to],
                                   sum))
  
  
  if(type == "allele") {
    
    basic_gene_table 
    
  }else if(type == "position") {
    
    basic_gene_table %>%
      dplyr::mutate(dplyr::across(2:ncol(.), ~ if_else(. )))
    
  }else if(type == "carrier") {
    
  }
    
}

count_vars_per_gene_per_indiv(data, 4, 5, type = "allele")

data  <- tibble::tibble(CHROM=1, 
                         POS=2:7, 
                         ID = ".", 
                         gene.col=c("g1", "g1", "g2", "g3", "g4","g4"), 
                         gt.col=c("0/1", "1/0", rep("0/0", 4)),
                         s2=c("1/0", "0/0", "1/0", "0/0", "0/1", "./."),
                         s3=c("0/0", "1/1", "0/0","0/0", "./.", "./."),
                         s4=c(rep("0/0", 3), "0/1", "1/1", "0/1"))

gt_col_from=5; gene_col=4; gt_col_to=ncol(data)
gene_col
typeof(gt_col_from)
