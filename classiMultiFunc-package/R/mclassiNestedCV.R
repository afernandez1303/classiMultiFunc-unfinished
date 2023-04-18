#'mclassiNestedCV
#'
#'mclassiInested
#'
#' @param cl: classifier
#' @param task: define a task with makeClassifTask
#' @param id: name of semi-metrics 
#' @param knn: value for nearest neighbor
#' @param h: value for kernel-based method 
#' @param par.vals: hyperparameters 
#' @param par.vals.ensemble: hyperparameters for either random forest or gradient boosting 
#' @param subset: ith inner sample 
#' @param cv: splits in the k-fold cross-validation in the inner loop. By default, 5-fold cross-validation is considered
#' @param set.seed: seed for the splits in the inner-loop
#' @param xreg: if covariates are further considered 
#'@export

mclassiInested = function ( cl ,
                            task , 
                            id ,
                            knn ,
                            h = NULL ,
                            par.vals ,
                            par.vals.ensemble = NULL , 
                            subset ,
                            cv = 5 ,
                            set.seed = NULL ,
                            xreg = NULL ) {
  # Need to add control parameters 
  if ( is.null ( task ) ) {
    stop ( "Error: task is NULL" ) }
  if ( is.null ( set.seed ) ) {
    isubset = sample ( cut ( 1 : length ( subset ) ,
                             breaks = cv , 
                             labels = 1 : cv ) )
    } else {
      set.seed ( set.seed , "L'Ecuyer" )
      isubset = sample ( cut ( 1 : length ( subset ) ,
                               breaks = cv , 
                               labels = 1 : cv ) ) }
  if ( !is.null ( knn ) ) {
    if ( length ( id ) == 1 ) {
      learner = mlr::makeLearner ( cl = cl ,
                              id = id ,
                              par.vals = list ( metric = par.vals$metric ,
                                                mdist  = par.vals$mdist [ ( ( 1 : task$task.desc$size ) %in% subset ) , 
                                                                          ( ( 1 : task$task.desc$size ) %in% subset ) ] ,
                                                knn = knn ,
                                                set.seed = par.vals$set.seed ) )
      } else {
        learner = mclassiEnsembleLearner ( cl = cl ,
                                           task = task ,
                                           id = id ,
                                           knn = knn ,
                                           h = NULL , 
                                           par.vals = par.vals , 
                                           subset = subset ) } 
    } else {
      if ( length ( id ) == 1 ) {
        learner = mlr::makeLearner ( cl = cl ,
                                id = id ,
                                par.vals = list ( metric = par.vals$metric ,
                                                  mdist = par.vals$mdist [ ( ( 1 : task$task.desc$size ) %in% subset ) ,
                                                                           ( ( 1 : task$task.desc$size ) %in% subset ) ] ,
                                                  kernel = par.vals$kernel ,
                                                  h = h , 
                                                  set.seed = par.vals$set.seed ) )
        } else {
          learner = mclassiEnsembleLearner ( cl = cl , 
                                             task = task , 
                                             id = id , 
                                             knn = NULL ,
                                             h = h ,
                                             par.vals = par.vals , 
                                             subset = subset ) } } 
  if ( length ( id ) == 1 ) {
    task = mlr::subsetTask ( task , 
                        subset = subset )
    model = apply ( cbind ( 1 : cv ) , 
                    1 , 
                    function ( x ) train ( learner = learner , 
                                           task = task ,
                                           subset = ( 1 : length ( subset ) ) [ isubset != x ] ) )
    } else {
      model = mclassiEnsembleTrain ( learner = learner , 
                                     task = task , 
                                     subset = subset , 
                                     cv = cv , 
                                     set.seed = set.seed ) }
  
  if ( length ( id ) == 1 ) {
    prediction = apply ( cbind ( 1 : cv ) , 
                         1 , 
                         function ( x ) predict ( model [[ x ]] , 
                                                  task = task , 
                                                  subset = ( 1 : length ( subset ) ) [ isubset == x ] ) )
    rreturn = apply ( cbind ( 1 : cv ) , 
                      1 , 
                      function ( x ) sum ( diag ( table ( prediction [[ x ]]$data [ , - 1 ] ) ) ) / sum ( table ( prediction [[ x ]]$data [ , - 1 ] ) ) )
    } else {
      prediction = mclassiEnsembleTest ( learner = learner , 
                                         model = model , 
                                         task = task )
      if ( par.vals.ensemble$ensemble.method == "Stacking" ) {
        rreturn = mclassiGenStack ( model = model , 
                                    prediction = prediction , 
                                    par.vals.ensemble = par.vals.ensemble , 
                                    xreg = xreg ) 
        rreturn = rreturn$y
      } else { 
        rreturn = mclassiLC ( model = model , 
                              prediction = prediction , 
                              par.vals.ensemble = par.vals.ensemble ) 
        rreturn = rreturn$X
        } 
      }
  return ( rreturn ) }

#' mclassiPerf
#' @param cl: classifier
#' @param task: define a task with makeClassifTask
#' @param id: name of semi-metrics 
#' @param knn: value for nearest neighbor
#' @param h: value for kernel-based method 
#' @param par.vals: hyperparameters 
#' @param par.vals.ensemble: hyperparameters for either random forest or gradient boosting 
#' @param subset: ith inner sample 
#' @param set.seed: seed for the splits in the inner-loop
#' @param xreg: if additional covariates are considered. Default NULL
#'@export
mclassiPerf = function ( cl ,
                         task ,
                         id ,
                         knn ,
                         h = NULL ,
                         par.vals ,
                         par.vals.ensemble = NULL , 
                         subset ,
                         cv = 5 , 
                         set.seed = NULL ,
                         xreg = NULL ) {
  if ( length ( id ) == 1 ) {
    if ( ! is.null ( knn ) ) {
      learner = mlr::makeLearner ( cl = cl , 
                              id = id , 
                              par.vals = list ( metric = par.vals$metric , 
                                                mdist = par.vals$mdist , 
                                                knn = knn ,
                                                set.seed = par.vals$set.seed ) )
    } else { 
      learner = mlr::makeLearner ( cl = cl , 
                              id = id ,
                              par.vals = list ( metric = par.vals$metric , 
                                                mdist = par.vals$mdist , 
                                                h = h , 
                                                kernel = kernel , 
                                                set.seed = par.vals$set.seed ) ) }
    model  = train ( learner = learner , 
                     task = task , 
                     subset = subset )
    prediction   = predict ( object = model , 
                             task = task , 
                             subset = ( 1 : task$task.desc$size ) [ ! ( ( 1 : task$task.desc$size ) %in% subset ) ] )
    accu = sum ( diag ( table ( prediction$data [ , - 1 ] ) ) ) / sum ( table ( prediction$data [ , - 1 ] ) ) 
  } else { 
    if ( ! is.null ( knn ) ) {
      learner = mclassiEnsembleLearner ( cl = cl , 
                                         task = task ,
                                         id = id ,
                                         knn = knn ,
                                         h = NULL , 
                                         par.vals = par.vals , 
                                         subset = subset )
      model = mclassiEnsembleTrain ( learner = learner , 
                                     task = task , 
                                     subset = subset , 
                                     cv = cv ,
                                     set.seed = set.seed )
      prediction  = mclassiEnsembleTest ( learner = learner , 
                                          model = model , 
                                          task = task )
      names ( prediction ) = c ( "id" , 
                                 paste ( "x" , 1 : ( ncol ( prediction ) - 2 ) , sep = "" ) , 
                                 "condition" )
      model2 = prediction2 = list ( )
      learner2 = mclassiEnsembleLearner ( cl = cl ,
                                          task = task , 
                                          id = id ,
                                          knn = knn ,
                                          h = NULL , 
                                          par.vals = par.vals , 
                                          subset = 1 : nrow ( task$env$data ) )
      for ( j in 1 : length ( id ) ) {
        model2 [[ j ]] = train ( learner = learner2 [[ j ]] ,
                                 task = task , 
                                 subset = subset )
        prediction2 [[ j ]] = data.frame ( id = which ( ! ( 1 : nrow ( task$env$data ) ) %in% subset ) ,
                                           pred = mlr::predictLearner ( .learner = learner2 [[ j ]] , 
                                                                   .model = model2 [[ j ]] , 
                                                                   .newdata = task$env$data [   which ( ! ( 1 : nrow ( task$env$data ) %in% subset ) ) , 
                                                                                                - which ( colnames ( task$env$data ) == "classes" ) ] ) ) }
      prediction2 = do.call ( cbind , prediction2 )
      prediction2 = prediction2 [ , c ( 1 , which ( colnames ( prediction2 ) != "id" ) ) ]
      prediction2 = data.frame ( doBy::orderBy ( ~ id , prediction2 ) )
      names ( prediction2 ) = c ( "id" , 
                                  paste ( "x" , 1 : ( ncol ( prediction2 ) - 1 ) , 
                                          sep = "" ) )
      if ( par.vals.ensemble$ensemble.method == "Stacking" ) {
        if ( !is.null ( xreg ) ) {
          prediction  = cbind ( prediction  , 
                                xreg [ prediction$id , - 1 ] )
          prediction2 = cbind ( prediction2 , 
                                xreg [ prediction2$id , - 1 ] ) }
        if ( par.vals.ensemble$super.learner == "randomForest" ) {
          modelStack = mlr::randomForest ( condition ~ . , data = prediction [ , - 1 ] ,
                                      ntree = as.numeric ( par.vals.ensemble$par.super.learner [ 1 ] ) ,
                                      mtry  = as.numeric ( par.vals.ensemble$par.super.learner [ 2 ] ) )
          prediStack = data.frame ( obs = task$env$data [ prediction2$id , "classes" ] , 
                                    pred = predict ( modelStack , 
                                                     prediction2 [ , - 1 ] ) )
          accu = sum ( diag ( table ( prediStack ) ) ) / sum ( table ( prediStack ) ) 
        } else {
          prediction [ , "condition" ] = as.numeric ( as.character ( prediction [ , "condition" ] ) )
          set.seed ( par.vals.ensemble$set.seed , 
                     "L'Ecuyer" )
          modelStack = mlr::gbm ( condition ~ . , 
                             data = prediction [ , - 1 ] ,
                             distribution = "bernoulli" ,
                             cv.folds = par.vals.ensemble$iters ,
                             n.trees = as.numeric ( par.vals.ensemble$par.super.learner [ 1 ] ) ,
                             interaction.depth = as.numeric ( par.vals.ensemble$par.super.learner [ 2 ] ) ,
                             shrinkage = as.numeric ( par.vals.ensemble$par.super.learner [ 3 ] ) )
          prediStack = mlr::predict.gbm ( object = modelStack , 
                                     newdata = prediction2 [ , - 1 ] ,
                                     n.trees = as.numeric ( par.vals.ensemble$par.super.learner [ 1 ] ) ,
                                     type = "response" )
          prediStack = data.frame ( obs = task$env$data [ prediction2$id , "classes" ] , 
                                    pred = ifelse ( prediStack <= 0.5 , 0 , 1 ) )
          accu = sum ( diag ( table ( prediStack ) ) ) / sum ( table ( prediStack ) ) 
        } } else {
          prediStack = data.frame ( obs = task$env$data [ prediction2$id , "classes" ] , 
                                    pred = ifelse ( as.matrix ( prediction2 ) [ , -1 ] [ , 1 : ncol ( as.matrix ( prediction2 ) [ , -1 ] ) %% 2 == 1 ] %*% 
                                                      matrix ( par.vals.ensemble$CL , ncol = 1 ) >= 0.5 , 0 , 1 ) )
          accu = sum ( diag ( table ( prediStack ) ) ) / sum ( table ( prediStack ) ) }
    } else {
      learner = mclassiEnsembleLearner ( cl = cl , 
                                         task = task ,
                                         id = id ,
                                         knn = NULL ,
                                         h = h , 
                                         par.vals = par.vals , 
                                         subset = subset )
      model = mclassiEnsembleTrain ( learner = learner , 
                                     task = task , 
                                     subset = subset , 
                                     cv = cv ,
                                     set.seed = set.seed )
      prediction  = mclassiEnsembleTest ( learner = learner , 
                                          model = model , 
                                          task = task )
      names ( prediction ) = c ( "id" , 
                                 paste ( "x" , 1 : ( ncol ( prediction ) - 2 ) , sep = "" ) , 
                                 "condition")
      model2 = prediction2 = list ( )
      learner2 = mclassiEnsembleLearner ( cl = cl , 
                                          task = task , 
                                          id = id ,
                                          knn = NULL ,
                                          h = h , 
                                          par.vals = par.vals , 
                                          subset = 1 : nrow ( task$env$data ) )
      for ( j in 1 : length ( id ) ) {
        model2 [[ j ]] = train ( learner = learner2 [[ j ]] ,
                                 task = task , 
                                 subset = subset )
        prediction2 [[ j ]] = data.frame ( id = which ( ! ( 1 : nrow ( task$env$data ) ) %in% subset ) ,
                                           pred = mlr::predictLearner ( .learner = learner2 [[ j ]] , 
                                                                   .model = model2 [[ j ]] , 
                                                                   .newdata = task$env$data [   which ( ! ( 1 : nrow ( task$env$data ) %in% subset ) ) , 
                                                                                                - which ( colnames ( task$env$data ) == "classes" ) ] ) ) }
      prediction2 = do.call ( cbind , prediction2 )
      prediction2 = prediction2 [ , c ( 1 , which ( colnames ( prediction2 ) != "id" ) ) ]
      prediction2 = data.frame ( doBy::orderBy ( ~ id , prediction2 ) )
      names ( prediction2 ) = c ( "id" , 
                                  paste ( "x" , 
                                          1 : ( ncol ( prediction2 ) - 1 ) , 
                                          sep = "" ) )
      if ( par.vals.ensemble$ensemble.method == "Stacking" ) {
        if ( par.vals.ensemble$super.learner == "randomForest" ) {
          modelStack = mlr::randomForest ( condition ~ . , data = prediction [ , - 1 ] ,
                                      ntree = as.numeric ( par.vals.ensemble$par.super.learner [ 1 ] ) ,
                                      mtry  = as.numeric ( par.vals.ensemble$par.super.learner [ 2 ] ) )
          prediStack = data.frame ( obs = task$env$data [ prediction2$id , "classes" ] , 
                                    pred = predict ( modelStack , 
                                                     prediction2 [ , - 1 ] ) )
          accu = sum ( diag ( table ( prediStack ) ) ) / sum ( table ( prediStack ) ) 
        } else {
          prediction [ , "condition" ] = as.numeric ( as.character ( prediction [ , "condition" ] ) )
          set.seed ( par.vals.ensemble$set.seed , 
                     "L'Ecuyer" )
          modelStack = mlr::gbm ( condition ~ . , 
                             data = prediction [ , - 1 ] ,
                             distribution = "bernoulli" ,
                             cv.folds = par.vals.ensemble$iters ,
                             n.trees = as.numeric ( par.vals.ensemble$par.super.learner [ 1 ] ) ,
                             interaction.depth = as.numeric ( par.vals.ensemble$par.super.learner [ 2 ] ) ,
                             shrinkage = as.numeric ( par.vals.ensemble$par.super.learner [ 3 ] ) )
          prediStack = mlr::predict.gbm ( object = modelStack , 
                                     newdata = prediction2 [ , - 1 ] ,
                                     n.trees = as.numeric ( par.vals.ensemble$par.super.learner [ 1 ] ) ,
                                     type = "response" )
          prediStack = data.frame ( obs = task$env$data [ prediction2$id , "classes" ] , 
                                    pred = ifelse ( prediStack <= 0.5 , 0 , 1 ) )
          accu = sum ( diag ( table ( prediStack ) ) ) / sum ( table ( prediStack ) ) }
      } else {
        prediStack = data.frame ( obs = task$env$data [ prediction2$id , "classes" ] , 
                                  pred = ifelse ( as.matrix ( prediction2 ) [ , -1 ] [ , 1 : ncol ( as.matrix ( prediction2 ) [ , -1 ] ) %% 2 == 1 ] %*% 
                                                    matrix ( par.vals.ensemble$CL , ncol = 1 ) >= 0.5 , 0 , 1 ) )
        accu = sum ( diag ( table ( prediStack ) ) ) / sum ( table ( prediStack ) ) }
    } 
  }
  return ( accu ) }

#'innerResultsExtract
#' @param x \code{vector}
#' @param h \code{vector}
#'@export
innerResultsExtract = function ( x , h ) {
  if ( is.null ( h ) ) {
    list ( sample = x [ length ( x ) ] ,
           knn = which.max ( x [ - length ( x ) ] ) ,
           accuracy = x [ which.max ( x [ - length ( x ) ] ) ] ) 
  } else {
    list ( sample = x [ length ( x ) ] ,
           h = h [ which.max ( x [ - length ( x ) ] ) ] ,
           accuracy = x [ which.max ( x [ - length ( x ) ] ) ] ) } }




