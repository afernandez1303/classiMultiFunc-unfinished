#' mmetricChoices
#'@export
mmetricChoices = function ( proxy.only = FALSE )
  {
  proxy.list = proxy :: pr_DB$get_entries ( )
  proxy_metric_names = c ( unlist ( sapply ( proxy.list [ c ( "Euclidean" , 
                                                              "Manhattan" ,
                                                              "Minkowski" ) ] , 
                                             function ( x ) x$names ) ) ,
                           "mean" ,
                           "globMax" ,
                           "globMin" ,
                           "globMaxU" ,
                           "globMinU" ,
                           "Frechet" ,
                           "Hausdorff" ,
                           "dtw" ,
                           "Aitchison" ,
                           "range" , 
                           "rangeU" , 
                           "dcor" , 
                           "NeedlemanWunsch" , 
                           "Levenshtein" , 
                           "Edit" ,
                           "Hamming" ,
                           "custom.metric" )
  return ( proxy_metric_names )
  }


