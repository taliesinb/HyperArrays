Package["HyperArrays`"]


PackageImport["GeneralUtilities`"]

Unprotect[Grid]
Grid[a_Association, opts:OptionsPattern[]] := Grid[KeyValueMap[Developer`ToList[{#1}, #2]&, a], opts, Alignment -> Left];
