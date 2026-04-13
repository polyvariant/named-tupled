package namedfunctions

import scala.quoted.*
import scala.annotation.publicInBinary

object NamedFunctions {

  /** Converts a multi-parameter function `(a: A, b: B, ...) => R` into a function with named
    * parameters `(a: A, b: B, ...) => R`, preserving the original parameter names.
    *
    * Usage: `NamedFunctions.of(someMethod)`
    */
  inline transparent def of[F](inline f: F): Any = ${ ofImpl('f) }

  /** Alias for `of`. Allows `NamedFunctions(someMethod)`. */
  inline transparent def apply[F](inline f: F): Any = ${ ofImpl('f) }

  /** Like `.tupled` but the resulting tuple type carries the parameter names from the original
    * method. Converts a multi-parameter function into a `Function1` from a named tuple.
    *
    * Usage: `NamedFunctions.tupled(someMethod)` or `someMethod.namedFunctions`
    */
  inline transparent def tupled[F](inline f: F): Any = ${ tupledImpl('f) }

  // A parameter clause: (names, types)
  // FuncInfo: (clauses, resultType)

  @publicInBinary
  private[namedfunctions] def extractInfo[F: Type](
    f: Expr[F]
  )(
    using q: Quotes
  ): (List[(List[String], List[q.reflect.TypeRepr])], q.reflect.TypeRepr) = {
    import q.reflect.*

    def extractParamClauses(term: Term): Option[List[List[String]]] =
      term match {
        case Inlined(_, _, expr) => extractParamClauses(expr)
        case Block(List(dd: DefDef), _: Closure) =>
          val clauses = dd.paramss.collect {
            case clause: TermParamClause => clause.params.map(_.name)
          }
          // Only recurse into the body if it's another closure (curried function)
          val innerClauses = dd.rhs.flatMap {
            case b @ Block(List(_: DefDef), _: Closure) => extractParamClauses(b)
            case _                                      => None
          }
          innerClauses match {
            case Some(inner) if clauses.nonEmpty => Some(clauses ++ inner)
            case _ if clauses.nonEmpty           => Some(clauses)
            case _                               => None
          }
        case Block(_, expr) => extractParamClauses(expr)
        case Lambda(params, body) =>
          val innerClauses = body match {
            case b @ Block(List(_: DefDef), _: Closure) => extractParamClauses(b)
            case _                                      => None
          }
          innerClauses match {
            case Some(rest) => Some(params.map(_.name) :: rest)
            case None       => Some(List(params.map(_.name)))
          }
        case ref if ref.symbol != null && ref.symbol.isDefDef =>
          val clausesList = ref.symbol.paramSymss.map(_.filter(_.isTerm))
          val nonEmpty = clausesList.filter(_.nonEmpty)
          if (nonEmpty.nonEmpty)
            Some(nonEmpty.map(_.map(_.name)))
          else
            None
        case _ => None
      }

    def unfoldFuncType(tpe: TypeRepr): (List[List[TypeRepr]], TypeRepr) =
      tpe match {
        case AppliedType(base, args) if base.typeSymbol.fullName.startsWith("scala.Function") =>
          val paramTypes = args.init
          val retType = args.last
          val (innerClauses, finalRet) = unfoldFuncType(retType)
          if (innerClauses.nonEmpty)
            (paramTypes :: innerClauses, finalRet)
          else
            (List(paramTypes), retType)
        case other => (Nil, other)
      }

    val funcTpe = TypeRepr.of[F].widenTermRefByName.dealias
    val (typeClauses, resultType) = unfoldFuncType(funcTpe)

    if (typeClauses.isEmpty)
      report.errorAndAbort(s"Expected a function type, got: ${funcTpe.show}")

    val nameClauses = extractParamClauses(f.asTerm).getOrElse {
      report.errorAndAbort(
        s"Could not extract parameter names from tree: ${f
            .asTerm
            .show(
              using Printer.TreeStructure
            )}. Pass an eta-expanded method reference (e.g. `obj.method`)."
      )
    }

    val alignedNameClauses =
      if (nameClauses.length == typeClauses.length)
        nameClauses
      else {
        // Try to split the flat name list according to type clause sizes
        val flatNames = nameClauses.flatten
        val sizes = typeClauses.map(_.length)
        if (flatNames.length == sizes.sum) {
          var remaining = flatNames
          sizes.map { size =>
            val (chunk, rest) = remaining.splitAt(size)
            remaining = rest
            chunk
          }
        } else {
          report.errorAndAbort(
            s"Mismatch: found ${nameClauses.map(_.length)} parameter name clauses but ${typeClauses.map(_.length)} parameter type clauses"
          )
        }
      }

    val clauses = alignedNameClauses.zip(typeClauses).map { case (names, types) =>
      if (names.length != types.length) {
        report.errorAndAbort(
          s"Mismatch: found ${names.length} parameter names but ${types.length} parameter types in a clause"
        )
      }
      (names, types)
    }

    (clauses, resultType)
  }

  /** Produces a FunctionN with named parameters (refined apply). */
  @publicInBinary
  private[namedfunctions] def ofImpl[F: Type](
    f: Expr[F]
  )(
    using q: Quotes
  ): Expr[Any] = {
    import q.reflect.*

    val (clauses, resultType) = extractInfo(f)

    def buildRefinedType(
      clauses: List[(List[String], List[TypeRepr])],
      retType: TypeRepr,
    ): TypeRepr =
      clauses match {
        case Nil => retType
        case (names, types) :: rest =>
          val innerType = buildRefinedType(rest, retType)
          val funcType = AppliedType(defn.FunctionClass(names.length).typeRef, types :+ innerType)
          Refinement(
            funcType,
            "apply",
            MethodType(names)(_ => types, _ => innerType),
          )
      }

    def buildLambda(
      owner: Symbol,
      clauses: List[(List[String], List[TypeRepr])],
      fTerm: Term,
      retType: TypeRepr,
    ): Term =
      clauses match {
        case Nil => fTerm
        case (names, types) :: rest =>
          val innerType = buildRefinedType(rest, retType)
          val mtpe = MethodType(names)(_ => types, _ => innerType)
          Lambda(
            owner,
            mtpe,
            { (meth, params) =>
              val args = params.map(_.asInstanceOf[Term])
              val funcType = AppliedType(
                defn.FunctionClass(names.length).typeRef,
                types :+ innerType,
              )
              val applyMethod = funcType.typeSymbol.methodMember("apply").head
              val applied = fTerm.select(applyMethod).appliedToArgs(args)
              if (rest.isEmpty) applied
              else buildLambda(meth, rest, applied, retType)
            },
          )
      }

    val fullType = buildRefinedType(clauses, resultType)
    val lambda = buildLambda(Symbol.spliceOwner, clauses, f.asTerm, resultType)

    Typed(
      lambda,
      TypeTree.of(using fullType.asType.asInstanceOf[Type[Any]]),
    ).asExprOf[Any]
  }

  @publicInBinary
  private[namedfunctions] def tupleTypeFromList(
    using q: Quotes
  )(
    types: List[q.reflect.TypeRepr]
  ): q.reflect.TypeRepr = {
    import q.reflect.*
    val n = types.length
    if (n == 0)
      report.errorAndAbort("NamedFunctions requires at least one parameter")
    AppliedType(Symbol.requiredClass(s"scala.Tuple$n").typeRef, types)
  }

  @publicInBinary
  private[namedfunctions] def namedTupleType(
    using q: Quotes
  )(
    names: List[String],
    types: List[q.reflect.TypeRepr],
  ): q.reflect.TypeRepr = {
    import q.reflect.*
    val namesTupleType = tupleTypeFromList(names.map(n => ConstantType(StringConstant(n))))
    val valuesTupleType = tupleTypeFromList(types)
    AppliedType(
      TypeRepr.of[NamedTuple.NamedTuple],
      List(namesTupleType, valuesTupleType),
    )
  }

  /** Produces a Function1 from a named tuple. */
  @publicInBinary
  private[namedfunctions] def tupledImpl[F: Type](
    f: Expr[F]
  )(
    using q: Quotes
  ): Expr[Any] = {
    import q.reflect.*

    val (clauses, resultType) = extractInfo(f)

    def buildTupledType(
      clauses: List[(List[String], List[TypeRepr])],
      retType: TypeRepr,
    ): TypeRepr =
      clauses match {
        case Nil => retType
        case (names, types) :: rest =>
          val ntType = namedTupleType(names, types)
          val innerType = buildTupledType(rest, retType)
          AppliedType(defn.FunctionClass(1).typeRef, List(ntType, innerType))
      }

    def buildFuncType(
      clauses: List[(List[String], List[TypeRepr])],
      retType: TypeRepr,
    ): TypeRepr =
      clauses match {
        case Nil => retType
        case (names, types) :: rest =>
          val innerType = buildFuncType(rest, retType)
          AppliedType(defn.FunctionClass(names.length).typeRef, types :+ innerType)
      }

    def extractArgs(ntRef: Term, types: List[TypeRepr]): List[Term] =
      types.zipWithIndex.map { case (pt, i) =>
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

    def buildTupledLambda(
      owner: Symbol,
      clauses: List[(List[String], List[TypeRepr])],
      fTerm: Term,
      retType: TypeRepr,
    ): Term =
      clauses match {
        case Nil => fTerm
        case (names, types) :: rest =>
          val ntType = namedTupleType(names, types)
          val innerType = buildTupledType(rest, retType)
          val mtpe =
            MethodType(List("key"))(
              _ => List(ntType),
              _ => innerType,
            )
          Lambda(
            owner,
            mtpe,
            { (meth, params) =>
              val ntRef = params.head.asInstanceOf[Term]
              val args = extractArgs(ntRef, types)
              val funcType = buildFuncType(clauses, retType)
              val applyMethod = funcType.typeSymbol.methodMember("apply").head
              val applied = fTerm.select(applyMethod).appliedToArgs(args)
              if (rest.isEmpty) applied
              else buildTupledLambda(meth, rest, applied, retType)
            },
          )
      }

    val fullType = buildTupledType(clauses, resultType)
    val lambda = buildTupledLambda(Symbol.spliceOwner, clauses, f.asTerm, resultType)

    Typed(
      lambda,
      TypeTree.of(using fullType.asType.asInstanceOf[Type[Any]]),
    ).asExprOf[Any]
  }

}

object syntax {

  extension [F](inline f: F) {
    inline transparent def named: Any = ${ NamedFunctions.ofImpl('f) }
    inline transparent def namedTupled: Any = ${ NamedFunctions.tupledImpl('f) }
  }

}
