package namedtupled

import scala.quoted.*
import scala.annotation.publicInBinary

object NamedTupled {

  /** Converts a multi-parameter function `(a: A, b: B, ...) => R` into a function with named
    * parameters `(a: A, b: B, ...) => R`, preserving the original parameter names.
    *
    * Usage: `NamedTupled.of(someMethod)`
    */
  inline transparent def of[F](inline f: F): Any = ${ ofImpl('f) }

  /** Alias for `of`. Allows `NamedTupled(someMethod)`. */
  inline transparent def apply[F](inline f: F): Any = ${ ofImpl('f) }

  /** Like `.tupled` but the resulting tuple type carries the parameter names from the original
    * method. Converts a multi-parameter function into a `Function1` from a named tuple.
    *
    * Usage: `NamedTupled.tupled(someMethod)` or `someMethod.namedTupled`
    */
  inline transparent def tupled[F](inline f: F): Any = ${ tupledImpl('f) }

  @publicInBinary
  private[namedtupled] def extractInfo[F: Type](
    f: Expr[F]
  )(
    using q: Quotes
  ): (List[String], List[q.reflect.TypeRepr], q.reflect.TypeRepr) = {
    import q.reflect.*

    def extractParamNames(term: Term): Option[List[String]] =
      term match {
        case Inlined(_, _, expr)                   => extractParamNames(expr)
        case Lambda(params, _)                     => Some(params.map(_.name))
        case Block(List(defn: DefDef), _: Closure) =>
          val names = defn.paramss.flatMap {
            case clause: TermParamClause => clause.params.map(_.name)
            case _                       => Nil
          }
          if (names.nonEmpty)
            Some(names)
          else
            None
        case Block(_, expr)                                   => extractParamNames(expr)
        case ref if ref.symbol != null && ref.symbol.isDefDef =>
          val syms = ref.symbol.paramSymss.flatten.filter(_.isTerm)
          if (syms.nonEmpty)
            Some(syms.map(_.name))
          else
            None
        case _ => None
      }

    val funcTpe = TypeRepr.of[F].widenTermRefByName.dealias
    val (paramTypes, resultType) =
      funcTpe match {
        case AppliedType(base, args) if base.typeSymbol.fullName.startsWith("scala.Function") =>
          (args.init, args.last)
        case other => report.errorAndAbort(s"Expected a function type, got: ${other.show}")
      }

    val paramNames = extractParamNames(f.asTerm).getOrElse {
      report.errorAndAbort(
        s"Could not extract parameter names from tree: ${f
            .asTerm
            .show(
              using Printer.TreeStructure
            )}. Pass an eta-expanded method reference (e.g. `obj.method`)."
      )
    }

    if (paramNames.length != paramTypes.length) {
      report.errorAndAbort(
        s"Mismatch: found ${paramNames.length} parameter names but ${paramTypes.length} parameter types"
      )
    }

    (paramNames, paramTypes, resultType)
  }

  /** Produces a FunctionN with named parameters (refined apply). */
  @publicInBinary
  private[namedtupled] def ofImpl[F: Type](
    f: Expr[F]
  )(
    using q: Quotes
  ): Expr[Any] = {
    import q.reflect.*

    val (paramNames, paramTypes, resultType) = extractInfo(f)
    val funcTpe = TypeRepr.of[F].widenTermRefByName.dealias

    val refinedType = Refinement(
      funcTpe,
      "apply",
      MethodType(paramNames)(_ => paramTypes, _ => resultType),
    )

    val mtpe = MethodType(paramNames)(_ => paramTypes, _ => resultType)

    val lambda = Lambda(
      Symbol.spliceOwner,
      mtpe,
      { (_, params) =>
        val args = params.map(_.asInstanceOf[Term])
        val applyMethod = funcTpe.typeSymbol.methodMember("apply").head
        f.asTerm.select(applyMethod).appliedToArgs(args)
      },
    )

    Typed(
      lambda,
      TypeTree.of(
        using refinedType.asType.asInstanceOf[Type[Any]]
      ),
    ).asExprOf[Any]
  }

  /** Produces a Function1 from a named tuple. */
  @publicInBinary
  private[namedtupled] def tupledImpl[F: Type](
    f: Expr[F]
  )(
    using q: Quotes
  ): Expr[Any] = {
    import q.reflect.*

    val (paramNames, paramTypes, resultType) = extractInfo(f)
    val funcTpe = TypeRepr.of[F].widenTermRefByName.dealias

    def tupleTypeFromList(types: List[TypeRepr]): TypeRepr = {
      val n = types.length
      if (n == 0)
        report.errorAndAbort("NamedTupled requires at least one parameter")
      AppliedType(Symbol.requiredClass(s"scala.Tuple$n").typeRef, types)
    }

    val namesTupleType = tupleTypeFromList(paramNames.map(n => ConstantType(StringConstant(n))))
    val valuesTupleType = tupleTypeFromList(paramTypes)
    val namedTupleType = AppliedType(
      TypeRepr.of[NamedTuple.NamedTuple],
      List(namesTupleType, valuesTupleType),
    )
    val function1Type = AppliedType(defn.FunctionClass(1).typeRef, List(namedTupleType, resultType))

    val mtpe =
      MethodType(List("key"))(
        _ => List(namedTupleType),
        _ => resultType,
      )

    val lambda = Lambda(
      Symbol.spliceOwner,
      mtpe,
      { (_, params) =>
        val ntRef = params.head.asInstanceOf[Term]
        val args = paramTypes.zipWithIndex.map { case (pt, i) =>
          Select
            .unique(
              Apply(
                Select.unique(Typed(ntRef, TypeTree.of[Product]), "productElement"),
                List(Literal(IntConstant(i))),
              ),
              "asInstanceOf",
            )
            .appliedToType(pt)
        }
        val applyMethod = funcTpe.typeSymbol.methodMember("apply").head
        f.asTerm.select(applyMethod).appliedToArgs(args)
      },
    )

    Typed(
      lambda,
      TypeTree.of(
        using function1Type.asType.asInstanceOf[Type[Any]]
      ),
    ).asExprOf[Any]
  }

}

object syntax {

  extension [F](inline f: F) {
    inline transparent def named: Any = ${ NamedTupled.ofImpl('f) }
    inline transparent def namedTupled: Any = ${ NamedTupled.tupledImpl('f) }
  }

}
