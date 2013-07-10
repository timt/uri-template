package org.tbag.uritemplate

import java.net.URLEncoder


class Expression(val expression: String) {
  //"{" [ operator ] variable-list "}"
  private val (operator, identifiers) = {
    expression take 1 match {
      case op@("+" | "#" | "." | "/" | ";" | "?" | "&") => (Some(op), expression.replace(op, ""))
      case _ => (None, expression)
    }
  }


  def expand(implicit variables: Map[String, Any]) = {
    applyOperator(identifiers.split(",").map(identifier => parseToValue(identifier, variables)))
  }


  private def parseToValue(identifier: String, variables: Map[String, Any]): (String, String) = {
    identifier.indexOf(":") match {
      case -1 => (identifier -> getValue(variables, identifier))
      case index => {
        val actualIdentifier = identifier.substring(0, index)
        val length = identifier.substring(index + 1).toInt
        (actualIdentifier -> getValue(variables, actualIdentifier, Some(length)))
      }
    }
  }


  private def getValue(variables: Map[String, Any], ident: String, length: Option[Int] = None): String = {
    val (identifier, explodeOp) = ident takeRight 1 match {
      case "*" => (ident dropRight 1, Some)
      case _ => (ident, None)
    }
    variables.getOrElse(identifier, "") match {
      case list: List[Any] => list.map(encode(_)) mkString chooseCollectionItemSeparator(explodeOp)
      case map: Map[Any, Any] => map.map {
        case (a, b) => encode(a) + {
          explodeOp match {
            case Some => "="
            case _ => ","
          }
        } + encode(b)
      } mkString chooseCollectionItemSeparator(explodeOp)
      case other => encode(length match {
        case Some(x) => other.toString.take(x)
        case _ => other.toString
      })
    }
  }


  private def chooseCollectionItemSeparator(explodeOp: Any): String = {
    explodeOp match {
      case Some => operator match {
        case Some(x) if (x == "." || x == "/") => x
        case _ => ","
      }
      case _ => ","
    }
  }

  private def applyOperator(values: Seq[(String, String)]) = {
    val valuesOnly = values.map(_._2)
    val retval: String = operator match {
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
    retval
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
    .replace("%3B", ";")
    .replace("%2C", ",")

  private def encode(value: Any) = URLEncoder.encode(value.toString, "UTF-8").replace("+", "%20")

}

object Expression {
  def apply(expression: String) = new Expression(expression.stripPrefix("{").stripSuffix("}"))

}

