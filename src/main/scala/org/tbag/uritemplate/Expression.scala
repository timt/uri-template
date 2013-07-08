package org.tbag.uritemplate

import java.net.URLEncoder


class Expression(val expression: String) {
  //"{" [ operator ] variable-list "}"
  private val (operator, identifiers) = expression.charAt(0).toString match {
    case op@("+" | "#") => (Some(op), expression.replace(op, ""))
    case _ => (None, expression)
  }


  def expand(variables: Map[String, Any]) = {
    applyOperator(identifiers.split(",").map(identifier => encode(variables.getOrElse(identifier, "").toString)))
  }

  private def applyOperator(values: Seq[String]) = {
    operator match {
      case Some("+") => reservedExpansion(mkString(values, "", ","))
      case Some("#") => reservedExpansion(mkString(values,"#", ","))
      case Some(".") => mkString(values, ".", ".")
      case Some(op) => values.mkString(",")
      case _ => values.mkString(",")
    }
  }

  private def mkString(values: Seq[String], prefix: String, separator: String) = {
    s"$prefix${values.mkString(separator)}"
  }


  private def reservedExpansion(value: String): String = value
    .replace("%21", "!")
    .replace("%2F", "/")

  private def encode(value: Any) = URLEncoder.encode(value.toString, "UTF-8").replace("+", "%20")

}

object Expression {
  def apply(expression: String) = new Expression(expression.stripPrefix("{").stripSuffix("}"))

}

