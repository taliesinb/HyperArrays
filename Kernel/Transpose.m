Package["HyperArrays`"]


PackageImport["GeneralUtilities`"]


PackageExport["InverseTranspose"]

SetUsage @ "
InverseTranspose[array$, {n$1, n$2, $$}] is the inverse of Transpose, specifying where n_i$ specifies\\
which level of the output the i$th level of the input should be transposed to.
"

(* this allows you to specify where the n'th level *goes*, instead of *comes from *)
InverseTranspose[array_, dims_] := Transpose[array, Ordering[dims]];
InverseTranspose[spec_][array_] := InverseTranspose[array, spec];


Unprotect[Transpose];

(* This moves the n'th level past the others to the m'th level *)
Transpose[array_, n_ -> m_] := Scope[
  If[m === -1, m = ArrayDepth[array]];
  If[n === -1, n = ArrayDepth[array]];
  If[n === m, Return[array]];
  levels = Range @ Max[m, n];
  levels = Insert[Delete[n] @ levels, n, If[m > n, m, m]];
  InverseTranspose[array, levels]
]

InverseTranspose[array_, n_ -> m_] :=
  Block[{InverseTranspose = Transpose}, Transpose[array, n -> m]];


PackageExport["ApplyTransposed"]

SetUsage @ "
ApplyTransposed[f$, x$] applies f$ to the transposed version of x$, then transposes back.
ApplyTransposed[spec$] represents the operator form of ApplyTransposed.
"

ApplyTransposed[f_, x_] := Transpose @ f @ Transpose @ x;
ApplyTransposed[f_][x_] := ApplyTransposed[f, x];

