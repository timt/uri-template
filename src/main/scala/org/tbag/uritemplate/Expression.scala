package org.tbag.uritemplate

import java.net.URLEncoder


class Expression(val expression: String) {
  //"{" [ operator ] variable-list "}"
  private val (operator, identifiers) = expression.charAt(0).toString match {
    case op@("+" | "#" | "." | "/" | ";" | "?" | "&") => (Some(op), expression.replace(op, ""))
    case _ => (None, expression)
  }


  def expand(implicit variables: Map[String, Any]) = {
    applyOperator(identifiers.split(",").map(identifier => (identifier -> encode(variables.getOrElse(identifier, "").toString))))
  }

  private def applyOperator(values: Seq[(String, String)]) = {
    val valuesOnly = values.toMap.values
    operator match {
      case Some("+") => reservedExpansion(mkString(valuesOnly, "", Some(",")))
      case Some("#") => reservedExpansion(mkString(valuesOnly, "#", Some(",")))
      case Some(".") => mkString(valuesOnly, ".")
      case Some("/") => mkString(valuesOnly, "/")
      case Some(";") => mkString(withIdentifiers(values, true), ";")
      case Some("?") => mkString(withIdentifiers(values), "?", Some("&"))
      case Some("&") => mkString(withIdentifiers(values), "&")
      case Some(op) => valuesOnly.mkString(",")
      case _ => valuesOnly.mkString(",")
    }
  }

  private def withIdentifiers(values: Seq[(String, String)], suppressSeparatorWhenEmpty: Boolean = false) = {
    values.map(keyValuePair => {
      val key = keyValuePair._1
      val value = keyValuePair._2
      val separator = value match {
        case "" if (suppressSeparatorWhenEmpty) => ""
        case _ => "="
      }
      key + separator + value
    })
  }

  private def mkString(values: Iterable[String], prefix: String, separator: Option[String] = None) = {
    s"$prefix${values.mkString(separator.getOrElse(prefix))}"
  }


  private def reservedExpansion(value: String): String = value
    .replace("%21", "!")
    .replace("%2F", "/")

  private def encode(value: Any) = URLEncoder.encode(value.toString, "UTF-8").replace("+", "%20")

}

object Expression {
  def apply(expression: String) = new Expression(expression.stripPrefix("{").stripSuffix("}"))

}

