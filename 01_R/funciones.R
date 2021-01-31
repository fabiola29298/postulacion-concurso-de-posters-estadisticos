### Funcion para etiquetar en base al archivo de SPSS

relabel_spss_variable <- function(x) {
  a <- base::attr(x = x, "labels") 
  if(!is.null(a)) {
    labels = base::names(a)
    levels = base::as.character(a)
    base::factor(x = x, levels = levels, labels = labels, ordered = TRUE) 
  } else {
    warning("x is not label. No relabelling done.")
    x
  }
}

### Grafico para barras de grupo

barras_grupo<- function(data, var_exp, var_group){
  data %>% 
    group_by({{var_group}}) %>% 
    count({{var_exp}}) %>% 
    ggplot(aes(y = {{var_exp}}, x = n)) +
    geom_col() +
    facet_wrap(vars({{var_group}}))
}
