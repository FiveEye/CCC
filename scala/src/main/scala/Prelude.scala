import de.fosd.typechef.featureexpr.{ FeatureExpr, SingleFeatureExpr, FeatureExprFactory }

import de.fosd.typechef.featureexpr.bdd.{ CastHelper, SatSolver, BDDFeatureExprFactory }

object Prelude {

  FeatureExprFactory.setDefault(BDDFeatureExprFactory)
  
  type Tag = SingleFeatureExpr

  type Formula = FeatureExpr
  
  def createTag(t: String): Tag = {
    FeatureExprFactory.createDefinedExternal(t)
  }

  val False = BDDFeatureExprFactory.False

  val True = BDDFeatureExprFactory.True  

}

