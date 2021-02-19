Package["HyperArrays`"]


PackageImport["GeneralUtilities`"]


PackageExport["Zeros"]

Zeros[shape: _Integer | {__Integer}] := ConstantArray[0, shape];
Zeros[shape_] := ConstantArray[0, Flatten @ List @ shape];


PackageExport["Ones"]

Ones[shape: _Integer | {__Integer}] := ConstantArray[1, shape];
Ones[shape_] := ConstantArray[1, Flatten @ List @ shape];


PackageExport["SymmetricRange"]

SymmetricRange[n_] := Range[-n, n];


PackageExport["GridRange"]

GridRange[sizes_List] := Tuples @ Map[Range, sizes];


PackageExport["SymbolicIndex"]

Format[SymbolicIndex[head_, indices___], StandardForm] :=
  Underscript[head, Column[formatIndex /@ {indices}, Center, 0]];

formatIndex[i_Integer] := i;
formatIndex[i:{__Integer}] := Row[MapIndexed[Style[#1, Part[$colors, Min[First @ #2, 9]]]&, i], "\[VeryThinSpace]"];

$colors = {Darker[Red], Darker[Green], Blue, Purple, Orange, Yellow, Brown, Gray, Black};


SymbolicIndex[head_, index___][next___] := SymbolicIndex[head, index, Flatten @ List @ next];


PackageExport["SymbolicArray"]

SymbolicArray[symbol_, shape_] := Array[SymbolicIndex[symbol], Flatten @ List @ shape];