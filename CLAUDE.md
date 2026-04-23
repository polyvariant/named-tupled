# named-functions

Scala 3 macro library for converting multi-parameter functions into functions with named parameters or named-tuple arguments.

## Build

Uses scala-cli (not sbt/mill). Scala 3.7.0 with `-no-indent` (braces style, no significant indentation).

```bash
# Run tests
scala-cli test .

# Format
scala-cli fmt .
```

## Project structure

- `project.scala` - scala-cli build config
- `NamedFunctions.scala` - all macro implementations + syntax extensions
- `NamedFunctions.test.scala` - munit tests

Single-module project. Everything is in the `namedfunctions` package.

## Code conventions

- Braces style Scala 3 (no significant indentation, enforced by scalafmt + compiler flag)
- Trailing commas on multi-line
- `@publicInBinary private[namedfunctions]` on internal macro impl methods
- Public API: `inline transparent def` methods in `NamedFunctions` object
- Syntax API: `object syntax` with `extension [F](inline f: F)` plus standalone helpers like `namedTuple`
- Tests use munit (`munit.FunSuite`)

## Architecture

Core pattern for function-transforming features:
1. Public `inline transparent def` in `NamedFunctions` object delegates to `${ implMethod('args) }`
2. `@publicInBinary private[namedfunctions]` impl method does the macro work
3. Extension method in `object syntax` calls the same impl

Standalone syntax helpers may skip step 1 or 3 depending on where they are exposed. For example,
`syntax.namedTuple` delegates directly to its macro implementation.

Key shared infrastructure:
- `extractInfo[F]` - extracts parameter clauses (names + types) and return type from a function expression
- `extractParamClauses` - gets parameter names from AST (handles DefDef, Lambda, Closure, symbol refs)
- `unfoldFuncType` - unfolds curried FunctionN types into clause list
- `namedTupleType` / `tupleTypeFromList` - helpers for building NamedTuple type representations

Refined types with `MethodType(names)` are used to preserve parameter names at call sites.
