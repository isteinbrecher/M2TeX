(*This is a package that enables mathematica to create *.tex files and create the Latex output.*)
BeginPackage["M2TeX`"];

(*Definition of Public Functions and Parameters*)
M2TeXToString::usage="Returns a string for the M2tex item.";
M2TeXSetEnvironment::usage="TODO";
M2TeXSetCommand::usage="TODO";
M2TeXSetPackage::usage="TODO";
M2TeXSetOption::usage="TODO";
M2TeXSetOptionalOption::usage="TODO";
M2TeXSetOptionList::usage="TODO";


(*Private Functions*)
Begin["`Private`"];


(* Clear the M2TeX variables*)
M2TeXClear := Module[{},
	Clear[M2TeXCurrentEnvironment,M2TeXEnvironmentList,M2TeXPreamble];
	M2TeXPreamble={};
];


(** M2TeXToString for general data types **)
M2TeXToString[item_] := ToString[item];
M2TeXToString[item_M2TeXNone] := "";



(** Options structure **)

(* To text function for options (the list overload is different ) *)
M2TeXToStringOptions[item_] := M2TeXToString[item];
M2TeXToStringOptions[item_List] := Module[
	{string = ""},
	
	(* Loop over items *)
	Do[
		string = string <> M2TeXToString[ item[[i]] ] <> If[i == Length[item], "", ", "];
	,{i, Length[item]}];
	
	(* Return string *)
	string
];


(* General Options Item *)
Options[M2TeXSetOption] = {
	"OpenCloseCharacter" -> {"{", "}"}
};
M2TeXSetOption[options_, OptionsPattern[]] := M2TeXOption[
	(* 1: Options in this item *)
	options,
	(* 2: Open and Close characters for the ToString function *)
	OptionValue["OpenCloseCharacter"]
];
M2TeXSetOption[options_M2TeXNone, OptionsPattern[]] := M2TeXNone[];

(* M2TeXToString function for commands *)
M2TeXToString[item_M2TeXOption] := Module[
	{string},
	
	(* get the string for the option *)
	string = item[[2,1]] <> M2TeXToStringOptions[item[[1]]] <> item[[2,2]];
	
	(* Return string of item *)
	string
];

(* Set optional parameters *)
M2TeXSetOptionalOption[options_] := M2TeXSetOption[options, "OpenCloseCharacter" -> {"[","]"}];



(** Options list **)
M2TeXSetOptionList[options_] := M2TeXOptionList[
	(* 1: Option List in this item *)
	options
];
M2TeXSetOptionList[options_M2TeXNone] := False;

(* M2TeXToString function for option lists *)
M2TeXToString[item_M2TeXOptionList] := Module[
	{string = ""},
	
	(* option list *)
	Do[
		string = string <> M2TeXToString[par];
	,{par, item[[1]]}];
	
	(* Return string of item *)
	string
];



(** Command structure **)

(* General Command Item *)
Options[M2TeXSetCommand] = {
	"Parameter" -> M2TeXNone[],
	"OptionalParameter" -> M2TeXNone[],
	"ParameterList" -> M2TeXNone[],
	"StartCharacter" -> "\\",
	"EndCharacter" -> "",
	"Header" -> {}
};
M2TeXSetCommand[name_, OptionsPattern[]] := Module[{},
	M2TexAddPreamble[OptionValue["Header"]];
	
	(* return the command item *)
	M2TeXCommand[
		(* 1: Name of environment *)
		name,
		(* 2: Parameters *)
		M2TeXSetOption[OptionValue["Parameter"]],
		(* 3: Optional parameters *)
		M2TeXSetOptionalOption[OptionValue["OptionalParameter"]],
		(* 4: Start character *)
		OptionValue["StartCharacter"],
		(* 5: End character *)
		OptionValue["EndCharacter"],
		(* 6: Multiple Options *)
		M2TeXSetOptionList[OptionValue["ParameterList"]]
	]		
]
M2TeXSetPackage[options__] := M2TeXSetCommand["usepackage", options];


(* M2TeXToString function for commands *)
M2TeXToString[item_M2TeXCommand] := Module[
	{string},
	
	(* get the string for the command *)
	string = item[[4]] <> item[[1]];
	
	(* get the options *)
	If[ item[[6]] === False ,
		string = string <> M2TeXToString[item[[3]]];
		string = string <> M2TeXToString[item[[2]]];
		,
		(* option list *)
		Print[item[[6]]];
		string = string <> M2TeXToString[item[[6]]];
	];
	string = string <> item[[5]];
	
	(* Return string of item *)
	string
];







(** Environment structure **)

(* General Command Item *)
Options[M2TeXSetEnvironment] = {
	"ParameterList" -> False,
	"Header" -> {},
	"NewLine" -> True
};
M2TeXSetEnvironment[name_, OptionsPattern[]] := Module[{},
	M2TexAddPreamble[OptionValue["Header"]];
	
	(* return the command item *)
	M2TeXEnvironment[
		(* 1: Name of environment *)
		name,
		(* 2: Contents of this environment *)
		{},
		(* 3: Start Command *)
		M2TeXSetCommand["begin", "Parameter" -> name],
		(* 4: End Command *)
		M2TeXSetCommand["end", "Parameter" -> name],
		(* 5: Options List *)
		If[False === OptionValue["ParameterList"],
			"",
			M2TeXSetOptionList[OptionValue["ParameterList"]]
		],
		(* 6: If newline should be made after the begin command *)
		OptionValue["NewLine"]
	]		
]


(* M2TeXToString function for commands *)
M2TeXToString[item_M2TeXEnvironment] := Module[
	{string},
	
	(* start the environment *)
	string = M2TeXToString[item[[3]]];
	
	(* add the parameters *)
	string = string <> M2TeXToString[item[[5]]];
	
	(* write new line *)
	If[item[[6]],
		string = string <> "\n";
	];
	
	(* Write the contents *)
	Do[
		string = string <> M2TeXToString[content];
	,{ content, item[[2]]}];
	
	
	(* write new line *)
	If[item[[6]],
		string = string <> "\n";
	];
	
	(* end the environment*)
	string = string <> M2TeXToString[item[[4]]];

	(* Return string of item *)
	string
];










(* End private *)
End[];

EndPackage[];