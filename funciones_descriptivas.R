
Descriptive_statistics <- function(base, heterogenidad, unidades_de_info,tratamiento, lista ) {
  library(tidyr)
  library(maditr )
  
  esta_desc = data.frame()
  esta_desc_heterogenities = data.frame()
  for (j in unique(base[[tratamiento]])  ) {
   
    TEMPORAL =  base %>% subset(base[[tratamiento]] == j)
    treated = j
    
    for (i in lista ) {
   
      temp = data.frame("variable" = i, 
                        "mean"  =  mean( TEMPORAL [[i]], na.rm = T )  ,
                        "median" = median(TEMPORAL[[i]], na.rm = T ) ,
                        "sd" = sd(TEMPORAL[[i]], na.rm = T ),
                        'N_indiviuos' = length(unique(TEMPORAL[[ unidades_de_info ]])),
                        "Observaciones" = length(TEMPORAL[[ unidades_de_info ]]) ,
                        "treat" = treated,
                        "heterogenidad" = 'All sample'
      )
      esta_desc = rbind(temp, esta_desc)
    }
   
  }
  
  esta_desc_heterogenities = data.frame()
  for (j in unique(base[[tratamiento]])  ) {
    TEMPORAL =  base %>% subset(base[[tratamiento]] == j)
    treated = j
  
    for (k in unique(TEMPORAL[[heterogenidad]]) ) {
      TEMPORAL_2=  subset(TEMPORAL, TEMPORAL[[heterogenidad]] == k)   
      
      for (l in lista ) {
        temp2 = data.frame("variable" = l, 
                           "mean"  =  mean( TEMPORAL_2 [[l]], na.rm = T )  ,
                           "median" = median(TEMPORAL_2[[l]], na.rm = T ) ,
                           "sd" = sd(TEMPORAL_2[[l]], na.rm = T ),
                           'N_indiviuos' = length(unique(TEMPORAL_2[[ unidades_de_info ]])),
                           "Observaciones" = length(TEMPORAL_2[[ unidades_de_info ]] ) ,
                           "treat" = treated, 
                           "heterogenidad" = k
        )
        esta_desc_heterogenities = rbind(temp2, esta_desc_heterogenities)
      }
      
    }
  }

estadisticas_descriptivas = rbind(esta_desc, esta_desc_heterogenities)
return(estadisticas_descriptivas)
}
