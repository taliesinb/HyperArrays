Package["HyperArrays`"]


PackageImport["GeneralUtilities`"]

(*

eventually this should be cartesian product of:

{,Positive,NonNegative}{Integer,Rational,Real,MachineReal}{Vector,Matrix,Array}

*)


PackageExport["IntegerVectorQ"]

IntegerVectorQ[{___Integer}] := True;
IntegerVectorQ[_] := False;


PackageExport["IntegerMatrixQ"]

IntegerMatrixQ[m_List /; MatrixQ[m, IntegerQ]] := True;
IntegerMatrixQ[_] := False;


