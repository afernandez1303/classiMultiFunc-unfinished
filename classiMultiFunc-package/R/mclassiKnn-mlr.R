#' MLR Implementation KNN
#' makeRLearner.classif.mclassiKnn
#' @export
makeRLearner.classif.mclassiKnn = function ( ) {
  makeRLearnerClassif (
    cl = "classif.mclassiKnn" ,
    package = "classiMultiFuncTest" ,
    par.set = makeParamSet (
      makeIntegerLearnerParam ( id = "knn" , 
                                default = 1L ,
                                lower = 1L , 
                                upper = 1000L ) ,
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
    # par.vals=list(keep.data=FALSE),
    properties = c ( "twoclass" ,
                     "multiclass" ,
                     "numerics" ,
                     "factors" ,
                     "prob" ) ,
    name = "Multivariate functional knn" ,
    short.name = "mclassiKnn" ,
    callees = "mclassiKnn" ) }

#' @export
trainLearner.classif.mclassiKnn = function ( .learner , .task , .subset , ... ) {
  ftask = getTaskDesc ( .task )
  data = getTaskData ( .task , .subset )
  mclassiKnn ( classes = data [ , ftask$target ] ,
               fdata = data [ , - which ( names ( data ) == ftask$target ) ] , ... ) }

#' @export
predictLearner.classif.mclassiKnn = function ( .learner , .model , .newdata , ... ) {
  if ( !is.null ( .model$learner.model$mdist ) ) {
    msubset = .model$learner.model$mdist [ !( ( 1 : ncol ( .model$learner$par.vals$mdist ) ) %in% .model$subset ) , .model$subset ]
  } else {
    msubset = NULL }
  predict.mclassiKnn ( object = .model$learner.model , 
                       newdata = .newdata ,
                       Mdist = msubset ,
                       predict.type = .model$learner$predict.type ) }



