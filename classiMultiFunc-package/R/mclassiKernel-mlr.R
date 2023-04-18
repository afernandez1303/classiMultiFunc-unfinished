#' MLR Implementation Kernel
#' makeRLearner.classif.mclassiKernel
#' @export
makeRLearner.classif.mclassiKernel = function ( ) 
  {
  makeRLearnerClassif (
    cl = "classif.mclassiKernel" ,
    package = "classiMultiFuncTest" ,
    par.set = makeParamSet (
      makeDiscreteLearnerParam ( id = "kernel" , 
                                 default = "Ker.norm" , 
                                 value = list ( "Ker.norm" ,
                                                "Ker.cos" ,
                                                "Ker.epa" ,
                                                "Ker.tri" ,
                                                "Ker.quar" ,
                                                "Ker.unif" , 
                                                NULL = NULL ) ) ,
      makeNumericLearnerParam ( id = "h" , 
                                default = 1 ) ,
      makeDiscreteLearnerParam ( id = "metric" , 
                                 default = "Euclidean" , 
                                 value = list ( "Euclidean" ,
                                                "L2" ,
                                                "Manhattan" ,
                                                "City-Block" ,
                                                "L1" ,
                                                "taxi" ,
                                                "Minkowski" , 
                                                "Lp" ,
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
                                                "custom.metric" ) ) ,
      makeIntegerLearnerParam ( id = "nderiv" , 
                                default = 0L , 
                                lower = 0L ) ,
      makeLogicalLearnerParam ( id = "parallel" , 
                                default = FALSE , 
                                tunable = FALSE ) ,
      makeIntegerLearnerParam ( id = "cl" , 
                                default = NULL , 
                                lower = 1L ,
                                special.vals = list ( NULL = NULL ) ,
                                tunable = FALSE ) ,
      makeLogicalLearnerParam ( id = "diag" , 
                                default = FALSE , 
                                tunable = FALSE ) ,
      makeLogicalLearnerParam ( id = "upper" , 
                                default = FALSE , 
                                tunable = FALSE ) ,
      makeUntypedLearnerParam ( id = "mdist" , 
                                default = NULL ) ,
      makeDiscreteLearnerParam ( id = "predict.type" , 
                                 default = "response" ,
                                 value = list ( "response" , 
                                                "prob" ) ) ,
      makeNumericLearnerParam ( id = "set.seed" , 
                                default = NULL ,
                                lower = 1 , 
                                special.vals = list ( NULL = NULL ) , 
                                tunable = FALSE ) ) ,
    properties = c ( "twoclass" ,
                     "multiclass" ,
                     "numerics" ,
                     "factors" ,
                     "prob" ) ,
    name = "Multivariate functional kernel" ,
    short.name = "mclassiKernel" ,
    callees = "mclassiKernel" ) 
  }

#'@export

trainLearner.classif.mclassiKernel = function( .learner , .task , .subset , ... ) {
  ftask = getTaskDesc ( .task )
  data = getTaskData ( .task , .subset )
   mclassiKernel ( classes = data [ , ftask$target ] ,
                   fdata = data [ , - which ( names ( data ) == ftask$target ) ] , ... ) }

#'@export
predictLearner.classif.mclassiKernel = function ( .learner , .model , .newdata , ... ) 
  {
  if ( !is.null ( .model$learner.model$mdist ) ) {
    msubset = .model$learner.model$mdist [ !( ( 1 : ncol ( .model$learner$par.vals$mdist ) ) %in% .model$subset ) , .model$subset ]
    } else {
    msubset = NULL }
  predict.mclassiKernel ( object = .model$learner.model , 
                          newdata = .newdata , 
                          Mdist = msubset , 
                          predict.type = .model$learner$predict.type ) 
  }

