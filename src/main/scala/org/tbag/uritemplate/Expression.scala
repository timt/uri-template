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
    applyOperator(identifiers.split(",").map(identifier => parseToValue(identifier, variables)).filter(entry => variableFilter(entry._2)))
  }

  private def variableFilter(value: Option[Any]) = {
    value match {
      case Some(item) => {
        item match {
          case null => false
          case map: Map[Any, Any] => !map.isEmpty
          case _ => true
        }
      }
      case None => false
    }
  }

  private def parseToValue(identifier: String, variables: Map[String, Any]): (String, Option[Any]) = {
    identifier.indexOf(":") match {
      case -1 => identifier -> getValue(variables, identifier)
      case index => {
        val actualIdentifier = identifier.substring(0, index)
        val length = identifier.substring(index + 1).toInt
        actualIdentifier -> getValue(variables, actualIdentifier, Some(length))
      }
    }
  }


  private def getValue(variables: Map[String, Any], ident: String, length: Option[Int] = None): Option[Any] = {
    val (identifier, _) = separateFromExplodeOp(ident)
    variables.get(identifier) match {
      case Some(value) => {
        length match {
          case Some(x) => Some(value.toString.take(x))
          case _ => Some(value)
        }
      }
      case _ => None
    }
  }


  private def separateFromExplodeOp(identifier: String): (String, Object) = {
    identifier takeRight 1 match {
      case "*" => (identifier dropRight 1, Some)
      case _ => (identifier, None)
    }
  }

  private def applyOperator(values: Seq[(String, Option[Any])]) = {
    val retval: String = operator match {
      case Some("+") => reservedExpansion(mkString(values, "", Some(",")))
      case Some("#") => reservedExpansion(mkString(values, "#", Some(",")))
      case Some(".") => mkString(values, ".")
      case Some("/") => mkString(values, "/")
      case Some(";") => mkString(values, ";", withIdentifiers = addIdentifiers(suppressSeparatorWhenEmpty = true))
      case Some("?") => mkString(values, "?", Some("&"), addIdentifiers())
      case Some("&") => mkString(values, "&", withIdentifiers = addIdentifiers())
      case _ => mkString(values, "", Some(","))
    }
    retval
  }

  private def addIdentifiers(suppressSeparatorWhenEmpty: Boolean = false) = (identifier: String, value: String) => {
    value match {
      case "" if suppressSeparatorWhenEmpty => identifier
      case _ => s"$identifier=$value"
    }
  }

  private val dontAddIdentifiers = (id: String, value: String) => value


  private def mkString(values: Seq[(String, Option[Any])], prefix: String, sep: Option[String] = None, withIdentifiers: (String, String) => String = dontAddIdentifiers) = {
    val separator = sep.getOrElse(prefix)
    val stringValues = values.map(value => {
      val (identifier, explodeOp) = separateFromExplodeOp(value._1)
      val underlyingValue: Any = value._2.get
      underlyingValue match {
        case list: List[Any] => {
          explodeOp match {
            case Some => list.map(value => withIdentifiers(identifier, encode(value))) mkString toCollectionSeparator(explodeOp, separator)
            case _ => withIdentifiers(identifier, list.map(encode(_)) mkString toCollectionSeparator(explodeOp, separator))
          }
        }
        case map: Map[Any, Any] => val mapAsString: String = map.map {
          case (a, b) => encode(a) + {
            explodeOp match {
              case Some => "="
              case _ => ","
            }
          } + encode(b)
        } mkString toCollectionSeparator(explodeOp, separator)
          explodeOp match {
            case Some => mapAsString
            case None => withIdentifiers(identifier, mapAsString)
          }
        case other => withIdentifiers(identifier, encode(other.toString))
      }
    })
    val retval = s"${
      if (stringValues.isEmpty) "" else prefix
    }${stringValues.mkString(separator) }"
    retval
  }


  private def toCollectionSeparator(explodeOp: Object, separator: String): String = {
    explodeOp match {
      case Some => separator
      case _ => ","
    }
  }

  private def reservedExpansion(value: String): String = value
    .replace("%21", "!")
    .replace("%2F", "/")
    .replace("%3B", ";")
    .replace("%2C", ",")
    .replace("%3A", ":")

  private def encode(value: Any) = URLEncoder.encode(value.toString, "UTF-8").replace("+", "%20")

}

object Expression {
  def apply(expression: String) = new Expression(expression.stripPrefix("{").stripSuffix("}"))

}

