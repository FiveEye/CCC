import de.fosd.typechef.featureexpr.{ FeatureExpr, SingleFeatureExpr, FeatureExprFactory }

import de.fosd.typechef.featureexpr.bdd.{ CastHelper, SatSolver, BDDFeatureExprFactory }

object Prelude {

  FeatureExprFactory.setDefault(BDDFeatureExprFactory)
  
  type Tag = SingleFeatureExpr

  type Formula = FeatureExpr
  
  def tag(t: String): Tag = {
    FeatureExprFactory.createDefinedExternal(t)
  }

  val False = FeatureExprFactory.False

  val True = FeatureExprFactory.True  

}

