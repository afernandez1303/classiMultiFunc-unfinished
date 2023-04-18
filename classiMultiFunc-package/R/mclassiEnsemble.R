#'mclassiEnsemble
#'
#' @param cl: classifier
#' @param task: define a task with makeClassifTask
#' @param id: name of semi-metrics 
#' @param knn: value for nearest neighbor
#' @param h: Default NULL. If not, bandwidth parameter for the kernel method 
#' @param par.vals: hyperparameters including metric name, mdist, kernel type, predict.type, set.seed (for ties)
#' @param subset: ith inner sample 
#'@export

mclassiEnsembleLearner = function ( cl ,
                                    task ,
                                    id ,
                                    knn , 
                                    h = NULL ,
                                    par.vals ,
                                    subset ) 
{
  checkmate :: assertChoice ( cl , 
                              choices = c ( "classif.mclassiKnn" , 
                                            "classif.mclassiKernel" ) )
  if ( is.null ( task ) ) {
    stop ( "Error: task is NULL" ) }
  if ( length ( id ) == 1 ) {
    stop ( "Error: id should be greater than or equal to 2" ) }
  learner = list ( )
  if ( ! is.null ( knn ) ) 
  {
    for ( j in 1 : length ( id ) ) {
      learner [[ j ]] = mlr::makeLearner ( cl = cl ,
                                      id = id [ j ] ,
                                      predict.type = par.vals [[ j ]]$predict.type ,
                                      par.vals = list ( metric = par.vals [[ j ]]$metric ,
                                                        mdist  = par.vals [[ j ]]$mdist [ ( ( 1 : task$task.desc$size ) %in% subset ) , 
                                                                                          ( ( 1 : task$task.desc$size ) %in% subset ) ] ,
                                                        knn = knn [ j ] ,
                                                        set.seed = par.vals [[ j ]]$set.seed ) ) }
  } else {
    for ( j in 1 : length ( id ) ) {
      learner [[ j ]] = mlr::makeLearner ( cl = cl ,
                                      id = id [ j ] ,
                                      predict.type = par.vals [[ j ]]$predict.type ,
                                      par.vals = list ( metric = par.vals [[ j ]]$metric ,
                                                        mdist  = par.vals [[ j ]]$mdist [ ( ( 1 : task$task.desc$size ) %in% subset ) , 
                                                                                          ( ( 1 : task$task.desc$size ) %in% subset ) ] ,
                                                        h = h [ j ] ,
                                                        kernel = par.vals [[ j ]]$kernel , 
                                                        set.seed = par.vals [[ j ]]$set.seed ) ) }
  }
  
  class ( learner ) = c ( "EnsembleLearner" )
  return ( learner )
}

#'mclassiEnsembleTrain
#'
#' @param learner: learner of class EnsembleLearner 
#' @param task: define a task with makeClassifTask
#' @param subset: ith inner sample 
#' @param cv: splits in the k-fold cross-validation in the inner loop. By default, 5-fold cross-validation is considered
#' @param set.seed: seed for the splits in the inner-loop
#'@export

mclassiEnsembleTrain = function ( learner ,
                                  task ,
                                  subset ,
                                  cv = 5 ,
                                  set.seed = NULL ) 
{
  if ( class ( learner ) != "EnsembleLearner" ) {
    stop ( "Error: learner object should be of class EnsembleLearner" ) }
  if ( is.null ( task ) ) {
    stop ( "Error: task is NULL" ) }
  model = list ( )
  if ( !is.null ( set.seed ) ) {
    set.seed ( set.seed , 
               "L'Ecuyer" )
    isubset = sample ( cut ( 1 : length ( subset ) , 
                             breaks = cv , 
                             labels = 1 : cv ) )
  } else {
    isubset = sample ( cut ( 1 : length ( subset ) , 
                             breaks = cv , 
                             labels = 1 : cv ) ) }
  task2 = mlr::subsetTask ( task , 
                       subset = subset )
  for ( j in 1 : length ( learner ) )
  {
    model [[ j ]] = list ( model = apply ( cbind ( 1 : cv ) , 
                                           1 , 
                                           function ( x ) 
                                             train ( learner = learner [[ j ]] , 
                                                     task = task2 , 
                                                     subset = ( 1 : length ( subset ) ) [ isubset != x ] ) ) , 
                           intern.vals = list ( subset = subset , 
                                                cv = cv , 
                                                set.seed = set.seed ) ) }
  class ( model ) = c ( "EnsembleTrain" )
  return ( model ) 
}

#'mclassiEnsembleTest
#'
#' @param learner: learner of class EnsembleLearner 
#' @param model: model of class EnsembleTrain
#' @param task: define a task with makeClassifTask
#'@export

mclassiEnsembleTest = function ( learner ,
                                 model ,
                                 task ) 
{
  if ( class ( learner ) != "EnsembleLearner" ) {
    stop ( "Error: learner object should be of class EnsembleLearner" ) }
  if ( class ( model ) != "EnsembleTrain" ) {
    stop ( "Error: model object should be of class EnsembleTrain" ) }
  if ( is.null ( task ) ) {
    stop ( "Error: task is NULL" ) }
  pred = list ( )
  if ( !is.null ( model [[ 1 ]]$intern.vals$set.seed ) ) {
    set.seed ( model [[ 1 ]]$intern.vals$set.seed , 
               "L'Ecuyer" )
    isubset = sample ( cut ( 1 : length ( model [[ 1 ]]$intern.vals$subset ) , 
                             breaks = model [[ 1 ]]$intern.vals$cv , 
                             labels = 1 : model [[ 1 ]]$intern.vals$cv ) )
  } else {
    isubset = sample ( cut ( 1 : length ( model [[ 1 ]]$intern.vals$subset ) , 
                             breaks = model [[ 1 ]]$intern.vals$cv , 
                             labels = 1 : model [[ 1 ]]$intern.vals$cv ) ) }
  task2 = mlr::subsetTask ( task , 
                       subset = model [[ 1 ]]$intern.vals$subset )
  for ( j in 1 : length ( learner ) ) {
    pred [[ j ]] = plyr::alply ( cbind ( 1 : model [[ j ]]$intern.vals$cv ) , 
                                 1 , 
                                 function ( x ) 
                                   data.frame ( id = model [[ j ]]$intern.vals$subset [ isubset == x ] ,
                                                pred = predict ( model [[ j ]]$model [[ x ]] ,
                                                                 task = task2 ,
                                                                 subset = ( 1 : length ( model[[ j ]]$intern.vals$subset ) ) [ isubset == x ] )$data [ , 3 : 4 ] ) ) 
  }
  pred = lapply ( pred , 
                  function ( x ) 
                    do.call ( rbind , x ) )
  pred = do.call ( cbind , 
                   pred )
  pred = pred [ , c ( 1 , 
                      which ( colnames ( pred ) != "id" ) ) ]
  pred = data.frame ( doBy :: orderBy ( ~ id , 
                                        pred ) )
  pred = data.frame ( pred , 
                      condition = task2$env$data$classes )
  names ( pred ) = c ( "id" , 
                       paste ( "v" , 1 : ( ncol ( pred ) - 2 ) , sep = "" ) , 
                       "condition")
  row.names ( pred ) = task$env$data [ model [[ 1 ]]$intern.vals$subset , "id" ]
  return ( pred ) }

#'mclassiGenStack
#'
#' @param model: model of class mclassiEnsembleTest
#' @param pred: predictions of an object of class mclassiEnsembleTest
#' @param par.vals.ensemble: list including super.learner (randomForest or boosting), iters for the k-fold cross-validation
#'related to the super.learner, and par.vals.ensemble setting of parameter candidates to be tuned using k-fold cross-validation, 
#'and set.seed for the splits of the k-fold cross-valdation to tune the super.learner
#' @param xreg: if additional covariates are considered. Default is NULL. 
#'@export

mclassiGenStack = function ( model ,
                             prediction ,
                             par.vals.ensemble ,
                             xreg = NULL ) 
{
  if ( class ( model ) != "EnsembleTrain" ) {
    stop ( "Error: model object should be of class EnsembleTrain" ) }
  checkmate :: assertChoice ( par.vals.ensemble$super.learner , 
                              choices = c ( "randomForest" , 
                                            "boosting" ) )
  if ( is.null ( xreg ) ) {
    leveloneData = pred
  } else {
    leveloneData = pred
    leveloneData$id.merge = row.names ( pred )
    leveloneData = merge ( leveloneData , 
                           xreg , 
                           by.x = "id.merge" , 
                           by.y = "id", 
                           all.x = TRUE)
    row.names ( leveloneData ) = leveloneData$id.merge
    leveloneData = doBy::orderBy ( ~ id , 
                             leveloneData ) [ , - 1 ] }
  if ( par.vals.ensemble$super.learner == "randomForest" ) 
  {
    learner = mlr::makeLearner ( cl = "classif.randomForest" ,
                            predict.type = "response" )
    task = mlr::makeClassifTask ( data = leveloneData [ , - 1 ] , 
                             target = "condition" )
    set.seed ( par.vals.ensemble$set.seed , 
               "L'Ecuyer" )
    par.ensemble = mlr::tuneParams ( learner = learner , 
                                task = task , 
                                resampling = mlr::makeResampleDesc ( "CV" , iters = par.vals.ensemble$iters ) , 
                                par.set = mlr::makeParamSet ( mlr::makeDiscreteParam ( "ntree" , 
                                                                             values = par.vals.ensemble$par.super.learner [ 1 ] ) ,
                                                         mlr::makeDiscreteParam ( "mtry" ,
                                                                             values = par.vals.ensemble$par.super.learner [ 2 ] ) ) , 
                                control = mlr::makeTuneControlGrid ( ) ,
                                measure = list ( acc ) )
  } else {
    learner = mlr::makeLearner ( cl = "classif.gbm" ,
                            predict.type = "response" ,
                            distribution = "bernoulli" )
    task = mlr::makeClassifTask ( data = leveloneData [ , - 1 ] ,
                             target = "condition" )
    set.seed ( par.vals.ensemble$set.seed , 
               "L'Ecuyer" )
    par.ensemble = mlr::tuneParams ( learner = learner , 
                                task = task , 
                                resampling = mlr::makeResampleDesc ( "CV" , iters = par.vals.ensemble$iters ) , 
                                par.set = mlr::makeParamSet ( mlr::makeDiscreteParam ( "n.trees" , 
                                                                             values = par.vals.ensemble$par.super.learner [ 1 ] ) ,
                                                         mlr::makeDiscreteParam ( "interaction.depth" ,
                                                                             values = par.vals.ensemble$par.super.learner [ 2 ] ) ,
                                                         mlr::makeDiscreteParam ( "shrinkage" , 
                                                                             values = par.vals.ensemble$par.super.learner [ 3 ] ) ) , 
                                control = mlr::makeTuneControlGrid ( ) , 
                                measure = acc )
  } 
  return ( par.ensemble ) 
}


#'mclassiCL
#'
#' @param model: model of class mclassiEnsembleTest
#' @param pred: predictions of an object of class mclassiEnsembleTest
#' @param par.vals.ensemble: parameters needed for least squares minimization with function lsei from package limSolve
#'@export

mclassiLC = function ( model ,
                       prediction ,
                       par.vals.ensemble ) {
  
  if ( class ( model ) != "EnsembleTrain" ) {
    stop ( "Error: model object should be of class EnsembleTrain" ) }
  
  Pmat = do.call ( rbind , 
                   apply ( cbind ( 1 : nrow ( prediction ) ) , 
                           1 , 
                           function ( x ) as.data.frame ( matrix ( prediction [ x , - which ( names ( prediction ) %in% c ( "id" , "condition" ) ) ] ,
                                                                   ncol = length ( model ) , 
                                                                   nrow = length ( unique ( prediction$condition ) ) ) ) ) )
  Pmat = as.matrix ( Pmat , 
                     ncol = ncol ( Pmat ) , 
                     nrow = nrow ( Pmat ) )
  Ivec = matrix ( unlist ( lapply ( lapply ( as.list ( prediction$condition ) , 
                                             function ( x ) table ( x ) ) , 
                                    function ( x ) as.vector ( x ) ) ) , 
                  ncol = 1 ) 
  
  wcoef = limSolve::lsei ( A = Pmat , 
                           B = Ivec , 
                           E = par.vals.ensemble$E , 
                           F = par.vals.ensemble$f , 
                           G = par.vals.ensemble$G , 
                           H = par.vals.ensemble$h )
  return ( wcoef ) }


