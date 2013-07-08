package org.tbag.uritemplate


case class URITemplate(template: String) {
  val expressionsRegex = """\{([^/]+?)\}""".r
  //  val expressions = (expressionsRegex findAllIn template).toList
  //  val queryExpression = expressions.find(_.startsWith("{?"))

  def expand(variables: (String, Any)*): String = {
    expressionsRegex.replaceAllIn(template, m => expandExpression(m.group(0), variables.toMap))
  }

  private def expandExpression(expression: String, variables: Map[String, Any]): String = {
    expandQueryParamsExpression(expression, variables)
  }


  def expandQueryParamsExpression(expression: String, variables: Map[String, Any]): String = {
    val identifiers = expression.substring(2, expression.length - 1).split(",")
    val expanded = identifiers.map(ident => variables.get(ident) match {
      case Some(value) => Some(s"${ident}=${value}")
      case None => None
    }).flatten
    expanded.size match {
      case 0 => ""
      case _ => s"?${expanded.mkString("&")}"
    }
  }
}




