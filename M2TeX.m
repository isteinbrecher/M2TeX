(*This is a package that enables mathematica to create *.tex files and create the Latex output.*)
BeginPackage["M2TeX`"];

(*Definition of Public Functions and Parameters*)
M2TeXToString::usage="Returns a string for the M2tex item.";
M2TeXSetCommand::usage="TODO";
M2TeXSetOption::usage="TODO";
M2TeXSetOptionalOption::usage="TODO";


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
M2TeXToString[item_List] := Module[
	{string = ""},
	
	(* Loop over items *)
	Do[
		string = string <> M2TeXToString[ item[[i]] ] <> If[i == Length[item], "", ", "];
	,{i, Length[item]}];
	
	(* Return string *)
	string
];




(** Options structure **)

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
M2TeXSetOption[options_M2TeXNone] := M2TeXNone[];

(* M2TeXToString function for commands *)
M2TeXToString[item_M2TeXOption] := Module[
	{string},
	
	(* get the string for the option *)
	string = item[[2,1]] <> M2TeXToString[item[[1]]] <> item[[2,2]];
	
	(* Return string of item *)
	string
];

(* Set optional parameters *)
M2TeXSetOptionalOption[options_] := M2TeXSetOption[options, "OpenCloseCharacter" -> {"[","]"}];


(** Command structure **)

(* General Command Item *)
Options[M2TeXSetCommand] = {
	"Parameter" -> M2TeXNone[],
	"OptionalParameter" -> M2TeXNone[],
	"ParameterList" -> False,
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
		OptionValue["ParameterList"]
	]		
]

(* M2TeXToString function for commands *)
M2TeXToString[item_M2TeXCommand] := Module[
	{string},
	
	(* get the string for the command *)
	string = item[[4]] <> item[[1]];
	
	(* get the options *)
	If[ item[[6]] === False ,
		string = string <> M2TeXToString[item[[2]]];
		string = string <> M2TeXToString[item[[3]]];
		,
		(* option list *)
		Do[
			string = string <> M2TeXToString[par];
		,{par, item[[6]]}];
	];
	string = string <> item[[5]];
	
	(* Return string of item *)
	string
];


Options[M2TexSetCommand]={"Parameter"->{},"OptionalParameter"->{},"EndCharacter"->"","StartCharacter"->"\\","Type"->M2TexCommand,"Header"->{}};
M2TexSetCommand[name_,OptionsPattern[]]:=Module[{},
M2TexAddPreamble[OptionValue["Header"]];
OptionValue["Type"][
name,(* Name of environment *)
If[M2TexParameter===Head@OptionValue["Parameter"],OptionValue["Parameter"],M2TexParameter[{{2,OptionValue["OptionalParameter"]},{1,OptionValue["Parameter"]}}]],(* Parameters and optional parameters *)
{OptionValue["StartCharacter"]},(* Character at the start of the command *)
{OptionValue["EndCharacter"]}(* Character at the end of the command *)
]
]







(* End private *)
End[];

EndPackage[];