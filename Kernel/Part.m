Package["HyperArrays`"]


PackageImport["GeneralUtilities`"]


PackageExport["ArrayTake"]

SetUsage @ "
ArrayTake[array$, spec$] is like Take, but expects a list of specifications.
ArrayTake[spec$] is the operator form of ArrayTake.
"

ArrayTake[array_List, spec_] := Take[array, spec];
ArrayTake[array_List, spec_List] := Take[array, Sequence @@ spec];
ArrayTake[spec_][array_List] := ArrayTake[array, spec];


PackageExport["ArrayPart"]

SetUsage @ "
ArrayPart[array$, spec$] is like Part, but expects a list of specifications.
ArrayPart[spec$] is the operator form of ArrayPart.
"

ArrayPart[array_List, spec_] := Part[array, spec];
ArrayPart[array_List, spec_List] := Part[array, Sequence @@ spec];
ArrayPart[spec_][array_List] := ArrayPart[array, spec];


PackageExport["ArrayDimension"]

SetUsage @ "
ArrayDimension[array$, n$] gives the dimension of the n$'th level.
"

ArrayDimension[array_, n_] := Part[Dimensions[array], n];


PackageExport["Every"]

SetUsage @ "
Every[n] evaluates to Span[All, All, n] (or ;; ;; n).
"

Every[n_] := Span[All, All, n];
Every[n_, offset_] := Span[offset, All, n];


PackageExport["ArrayDecimate"]

SetUsage @ "
ArrayDecimate[array$, n$] will take every n$ elements from an array.
ArrayDecimate[array$, {n$1, n$2, $$}] applies n$i at level $i.
* specs can be given as rules of the form i$ -> n_i$
"

decimate[array_, level_, n_Integer] := ArrayPart[array, Append[ConstantArray[All, level - 1], Every[n]]];
decimate[array_, level_, Into[n_]] := decimate[array, level, Ceiling[ArrayDimension[array, level] / n]];
decimate[array_, level_, 1|None] := array;
(*
decimate[array_, level_, BlockMapped[f_, n_Integer]] := BlockMap[f, array, level -> n];
*)

(* single spec*)
ArrayDecimate[array_List, spec_] := decimate[array, 1, spec];

(* list of specs {spec_1, spec_2, ...} *)
ArrayDecimate[array_List, spec_List] := Scope[
  Do[array = decimate[array, i, spec[[i]]], {i, Length[spec]}];
  array
];

(* association of level \[Rule] spec *)
ArrayDecimate[array_List, spec_Association] := Scope[
  KeyValueScan[Function[array = decimate[array, #1, #2]], spec];
  array
];

(* operator form *)
ArrayDecimate[spec_][array_List] := ArrayDecimate[array, spec];

