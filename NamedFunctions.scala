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

  /** Builds a named tuple from variables or field accesses by using their names as labels.
    *
    * Usage: `NamedFunctions.namedTuple(foo, bar)` which expands to `(foo = foo, bar = bar)`.
    */
  inline transparent def namedTuple(inline args: Any*): Any = ${ namedTupleImpl('args) }

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
        case Inlined(_, _, expr)                 => extractParamClauses(expr)
        case Block(List(dd: DefDef), _: Closure) =>
          val clauses = dd.paramss.collect { case clause: TermParamClause =>
            clause.params.map(_.name)
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
        case Block(_, expr)       => extractParamClauses(expr)
        case Lambda(params, body) =>
          val innerClauses =
            body match {
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
        case _ =>
          // Fallback: try to extract names from refined FunctionN types
          // e.g. Function2[A, B, R] { def apply(a: A, b: B): R }
          def extractFromRefinedType(tpe: TypeRepr): Option[List[List[String]]] =
            tpe.dealias match {
              case Refinement(parent, "apply", MethodType(names, _, retTpe)) =>
                val innerClauses = extractFromRefinedType(retTpe)
                innerClauses match {
                  case Some(rest) => Some(names :: rest)
                  case None       => Some(List(names))
                }
              case _ => None
            }
          extractFromRefinedType(TypeRepr.of[F].widenTermRefByName.dealias)
      }

    def unfoldFuncType(tpe: TypeRepr): (List[List[TypeRepr]], TypeRepr) =
      tpe match {
        case Refinement(parent, _, _) => unfoldFuncType(parent)
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
        case Nil                    => retType
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
        case Nil                    => fTerm
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
              if (rest.isEmpty)
                applied
              else
                buildLambda(meth, rest, applied, retType)
            },
          )
      }

    val fullType = buildRefinedType(clauses, resultType)
    val lambda = buildLambda(Symbol.spliceOwner, clauses, f.asTerm, resultType)

    Typed(
      lambda,
      TypeTree.of(
        using fullType.asType.asInstanceOf[Type[Any]]
      ),
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

  @publicInBinary
  private[namedfunctions] def unwrapInlined(
    using q: Quotes
  )(
    term: q.reflect.Term
  ): q.reflect.Term = {
    import q.reflect.*
    term match {
      case Inlined(_, _, inner) => unwrapInlined(inner)
      case other                => other
    }
  }

  @publicInBinary
  private[namedfunctions] def unpackVarargs(
    using q: Quotes
  )(
    args: Expr[Seq[Any]],
    methodName: String,
  ): List[q.reflect.Term] = {
    import q.reflect.*
    unwrapInlined(args.asTerm) match {
      case Typed(Repeated(elems, _), _) => elems
      case Repeated(elems, _)           => elems
      case other                        =>
        report.errorAndAbort(
          s"$methodName: unexpected args tree shape: ${other.show(
              using Printer.TreeStructure
            )}"
        )
    }
  }

  @publicInBinary
  private[namedfunctions] def extractArgName(
    using q: Quotes
  )(
    term: q.reflect.Term,
    methodName: String,
  ): String = {
    import q.reflect.*
    unwrapInlined(term) match {
      case Ident(name)     => name
      case Select(_, name) => name
      case other           =>
        report.errorAndAbort(
          s"$methodName requires variable references or field accesses as arguments, got: ${other.show}"
        )
    }
  }

  @publicInBinary
  private[namedfunctions] def namedTupleImpl(
    args: Expr[Seq[Any]]
  )(
    using q: Quotes
  ): Expr[Any] = {
    import q.reflect.*

    val argTerms = unpackVarargs(args, "namedTuple")
    if (argTerms.isEmpty)
      report.errorAndAbort("namedTuple requires at least one argument")

    val argNames = argTerms.map(extractArgName(_, "namedTuple"))
    val duplicates = argNames.groupBy(identity).collect { case (n, vs) if vs.length > 1 => n }.toList
    if (duplicates.nonEmpty)
      report.errorAndAbort(s"namedTuple: duplicate argument names: ${duplicates.mkString(", ")}")

    val argTypes = argTerms.map(_.tpe.widenTermRefByName)
    val tupleModule = Symbol.requiredModule(s"scala.Tuple${argTerms.length}")
    val tupleApply = tupleModule.methodMember("apply").head
    val tuple = Ref(tupleModule)
      .select(tupleApply)
      .appliedToTypes(argTypes)
      .appliedToArgs(argTerms)

    val ntType = namedTupleType(argNames, argTypes)
    Typed(
      tuple,
      TypeTree.of(
        using ntType.asType.asInstanceOf[Type[Any]]
      ),
    ).asExprOf[Any]
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
        case Nil                    => retType
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
        case Nil                    => retType
        case (names, types) :: rest =>
          val innerType = buildFuncType(rest, retType)
          AppliedType(defn.FunctionClass(names.length).typeRef, types :+ innerType)
      }

    def extractArgs(ntRef: Term, types: List[TypeRepr]): List[Term] = types.zipWithIndex.map {
      case (pt, i) =>
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
        case Nil                    => fTerm
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
              if (rest.isEmpty)
                applied
              else
                buildTupledLambda(meth, rest, applied, retType)
            },
          )
      }

    val fullType = buildTupledType(clauses, resultType)
    val lambda = buildTupledLambda(Symbol.spliceOwner, clauses, f.asTerm, resultType)

    Typed(
      lambda,
      TypeTree.of(
        using fullType.asType
      ),
    ).asExprOf[Any]
  }

  /** Applies a function after checking that each argument's variable name matches the corresponding
    * parameter name. Fails at compile time if names don't match.
    *
    * Usage: `someMethod.nameChecked(matchingVar1, matchingVar2)`
    */
  inline transparent def nameChecked[F](inline f: F)(inline args: Any*): Any =
    ${
      nameCheckedImpl('f, 'args)
    }

  /** Applies a function using fields from a product (case class) matched by name.
    *
    * Usage: `someMethod.applyProduct(someCaseClass)`
    */
  inline transparent def applyProduct[F, P](inline f: F)(inline p: P): Any =
    ${
      applyProductImpl('f, 'p)
    }

  /** Converts a `Function1` from a named tuple back into a multi-parameter function with named
    * parameters. Reverses the process of `tupled`.
    *
    * Usage: `someFunction.namedUntupled`
    */
  inline transparent def untupled[F](inline f: F): Any = ${ untupledImpl('f) }

  @publicInBinary
  private[namedfunctions] def untupledImpl[F: Type](
    f: Expr[F]
  )(
    using q: Quotes
  ): Expr[Any] = {
    import q.reflect.*

    val funcTpe = TypeRepr.of[F].widenTermRefByName.dealias

    // Extract Function1[NamedTuple[Names, Values], R]
    val (namedTupleTpe, resultType) =
      funcTpe match {
        case AppliedType(base, List(paramType, retType))
            if base.typeSymbol.fullName == "scala.Function1" =>
          (paramType.dealias, retType)
        case other =>
          report.errorAndAbort(
            s"namedUntupled requires a Function1 from a named tuple, got: ${other.show}"
          )
      }

    // Extract NamedTuple.NamedTuple[Names, Values]
    val (namesTpe, valuesTpe) =
      namedTupleTpe match {
        case AppliedType(base, List(names, values))
            if base.typeSymbol.fullName == "scala.NamedTuple$.NamedTuple" =>
          (names, values)
        case other =>
          report.errorAndAbort(
            s"namedUntupled requires a Function1 from a named tuple, got parameter type: ${other.show}"
          )
      }

    // Extract names from the Names tuple type
    def extractNames(tpe: TypeRepr): List[String] =
      tpe.dealias match {
        case AppliedType(_, args) =>
          args.map {
            case ConstantType(StringConstant(name)) => name
            case other => report.errorAndAbort(s"Expected string constant type, got: ${other.show}")
          }
        case other => report.errorAndAbort(s"Expected tuple type for names, got: ${other.show}")
      }

    // Extract types from the Values tuple type
    def extractTypes(tpe: TypeRepr): List[TypeRepr] =
      tpe.dealias match {
        case AppliedType(_, args) => args
        case other => report.errorAndAbort(s"Expected tuple type for values, got: ${other.show}")
      }

    val paramNames = extractNames(namesTpe)
    val paramTypes = extractTypes(valuesTpe)

    if (paramNames.length != paramTypes.length)
      report.errorAndAbort(
        s"Mismatch: ${paramNames.length} names but ${paramTypes.length} types in named tuple"
      )

    // Build FunctionN with refined apply
    val funcNType = AppliedType(
      defn.FunctionClass(paramNames.length).typeRef,
      paramTypes :+ resultType,
    )
    val refinedType = Refinement(
      funcNType,
      "apply",
      MethodType(paramNames)(_ => paramTypes, _ => resultType),
    )

    val mtpe = MethodType(paramNames)(_ => paramTypes, _ => resultType)

    val lambda = Lambda(
      Symbol.spliceOwner,
      mtpe,
      { (_, params) =>
        val args = params.map(_.asInstanceOf[Term])
        // Build a named tuple from the args and call the original function
        val tupleModule = Symbol.requiredModule(s"scala.Tuple${paramNames.length}")
        val tupleApply = tupleModule.methodMember("apply").head
        val tuple = Ref(tupleModule)
          .select(tupleApply)
          .appliedToTypes(paramTypes)
          .appliedToArgs(args)
        // Cast tuple to the named tuple type
        val namedTuple = Typed(
          tuple,
          TypeTree.of(
            using namedTupleTpe.asType.asInstanceOf[Type[Any]]
          ),
        )
        // Call f.apply(namedTuple)
        val applyMethod = defn.FunctionClass(1).methodMember("apply").head
        f.asTerm.select(applyMethod).appliedToArgs(List(namedTuple))
      },
    )

    Typed(
      lambda,
      TypeTree.of(
        using refinedType.asType.asInstanceOf[Type[Any]]
      ),
    ).asExprOf[Any]
  }

  /** Shared helper: applies a function term to a flat list of argument terms, walking curried
    * parameter clauses.
    */
  @publicInBinary
  private[namedfunctions] def buildFuncApplication(
    using q: Quotes
  )(
    fTerm: q.reflect.Term,
    clauses: List[(List[String], List[q.reflect.TypeRepr])],
    allArgs: List[q.reflect.Term],
    resultType: q.reflect.TypeRepr,
  ): q.reflect.Term = {
    import q.reflect.*

    def innerRetType(
      remaining: List[(List[String], List[TypeRepr])],
      retType: TypeRepr,
    ): TypeRepr =
      remaining match {
        case Nil                    => retType
        case (names, types) :: rest =>
          val inner = innerRetType(rest, retType)
          AppliedType(defn.FunctionClass(names.length).typeRef, types :+ inner)
      }

    clauses match {
      case Nil                    => fTerm
      case (names, types) :: rest =>
        val (theseArgs, remainingArgs) = allArgs.splitAt(names.length)
        val retTpe = innerRetType(rest, resultType)
        val funcType = AppliedType(defn.FunctionClass(names.length).typeRef, types :+ retTpe)
        val applyMethod = funcType.typeSymbol.methodMember("apply").head
        val applied = fTerm.select(applyMethod).appliedToArgs(theseArgs)
        buildFuncApplication(applied, rest, remainingArgs, resultType)
    }
  }

  @publicInBinary
  private[namedfunctions] def nameCheckedImpl[F: Type](
    f: Expr[F],
    args: Expr[Seq[Any]],
  )(
    using q: Quotes
  ): Expr[Any] = {
    import q.reflect.*

    val (clauses, resultType) = extractInfo(f)
    val argTerms = unpackVarargs(args, "nameChecked")

    val flatParamNames = clauses.flatMap(_._1)
    val argsByName: Map[String, Term] =
      argTerms.map { term =>
        val name = extractArgName(term, "nameChecked")
        name -> term
      }.toMap

    val argNames = argTerms.map(extractArgName(_, "nameChecked"))

    if (argsByName.size != argTerms.length) {
      val dupes = argNames.groupBy(identity).collect { case (n, vs) if vs.length > 1 => n }
      report.errorAndAbort(
        s"nameChecked: duplicate argument names: ${dupes.mkString(", ")}"
      )
    }

    if (argTerms.length != flatParamNames.length)
      report.errorAndAbort(
        s"nameChecked: expected ${flatParamNames.length} arguments but got ${argTerms.length}"
      )

    // Check that argument names are an exact match for parameter names
    val paramNameSet = flatParamNames.toSet
    val argNameSet = argsByName.keySet
    val unexpected = argNameSet -- paramNameSet
    val missing = paramNameSet -- argNameSet
    if (unexpected.nonEmpty || missing.nonEmpty) {
      val parts =
        List(
          if (unexpected.nonEmpty)
            Some(s"unexpected: ${unexpected.mkString(", ")}")
          else
            None,
          if (missing.nonEmpty)
            Some(s"missing: ${missing.mkString(", ")}")
          else
            None,
        ).flatten
      report.errorAndAbort(s"nameChecked: ${parts.mkString("; ")}")
    }

    // Reorder arguments to match parameter order
    val reorderedArgs = flatParamNames.map(argsByName)
    val result = buildFuncApplication(f.asTerm, clauses, reorderedArgs, resultType)
    result.asExprOf[Any]
  }

  @publicInBinary
  private[namedfunctions] def applyProductImpl[F: Type, P: Type](
    f: Expr[F],
    p: Expr[P],
  )(
    using q: Quotes
  ): Expr[Any] = {
    import q.reflect.*

    val (clauses, resultType) = extractInfo(f)

    val productType = TypeRepr.of[P].widenTermRefByName.dealias
    val productSymbol = productType.typeSymbol
    val caseFields = productSymbol.caseFields

    val fieldsByName: Map[String, Symbol] = caseFields.map(f => f.name -> f).toMap

    // For each parameter, find the matching field
    val allArgs: List[Term] = clauses.flatMap { case (names, types) =>
      names.zip(types).map { case (paramName, paramType) =>
        fieldsByName.get(paramName) match {
          case None =>
            report.errorAndAbort(
              s"applyProduct: product type ${productType.show} has no field named '$paramName'"
            )
          case Some(fieldSym) =>
            val fieldType = productType.memberType(fieldSym)
            if (!(fieldType <:< paramType))
              report.errorAndAbort(
                s"applyProduct: field '$paramName' has type ${fieldType.show} but parameter expects ${paramType.show}"
              )
            p.asTerm.select(fieldSym)
        }
      }
    }

    val result = buildFuncApplication(f.asTerm, clauses, allArgs, resultType)
    result.asExprOf[Any]
  }

}
object syntax {

  inline transparent def namedTuple(inline args: Any*): Any = ${ NamedFunctions.namedTupleImpl('args) }

  extension [F](inline f: F) {
    inline transparent def named: Any = ${ NamedFunctions.ofImpl('f) }
    inline transparent def namedTupled: Any = ${ NamedFunctions.tupledImpl('f) }
    inline transparent def namedUntupled: Any = ${ NamedFunctions.untupledImpl('f) }

    inline transparent def nameChecked(inline args: Any*): Any =
      ${
        NamedFunctions.nameCheckedImpl('f, 'args)
      }

    inline transparent def applyProduct[P](inline p: P): Any =
      ${
        NamedFunctions.applyProductImpl('f, 'p)
      }

  }

}
