(*This is a package that enables mathematica to create *.tex files and create the Latex output.*)
BeginPackage["M2TeX`"];

(*Definition of Public Functions and Parameters*)
M2TeXToString::usage="Returns a string for the M2tex item.";
M2TeXSetDocument::usage="TODO";
M2TeXSetEnvironment::usage="TODO";
M2TeXSetCommand::usage="TODO";
M2TeXSetPackage::usage="TODO";
M2TeXSetOption::usage="TODO";
M2TeXSetOptionalOption::usage="TODO";
(*M2TeXSetOptionList::usage="TODO";*)
M2TeXDocumentToString::usage="TODO";
M2TeXAppendToDocument::usage="TODO";
M2TeXCloseEnvironment::usage="TODO";
M2TexAddPreamble::usage="TODO";
M2TeXGeneratePdf::usage="TODO";
M2TeXSetPlot::usage="TODO";





M2TeXOption::usage="TODO";
M2TeXOptionOptional::usage="TODO";
M2TeXOptionList::usage="TODO";

M2TeXCommand::usage="TODO";
M2TeXPackage::usage="TODO";
M2TeXEnvironment::usage="TODO";


(*Private Functions*)
Begin["`Private`"];






(********* Helper and compatibility functions *********)

(*True if file exists and is not a directory*)
fileQ[file_] := FileType[file] === File








(*







(* Clear the M2TeX variables*)
M2TeXClear := Clear[
	m2texDocument,
	m2texCurrentEnvironment
];



(** M2TeX document **)
Options[M2TeXSetDocument] = {
	"DocumentClass" -> M2TeXSetCommand[
		"documentclass",
		"Parameter" -> "scrartcl"
	]	
};
M2TeXSetDocument[OptionsPattern[]] := (
	M2TeXClear;
	
	m2texDocument = M2TeXDocument[
		(* 1: The document class *)
		OptionValue["DocumentClass"],
		(* 2: Preamble *)
		{},
		(* 3: Contents *)
		M2TeXSetEnvironment["document"]
	];
	
	m2texCurrentEnvironment = {3};
);

(* M2TeXToString function for document *)
M2TeXToString[item_M2TeXDocument] := Module[
	{string},
	
	(* add the document class *)
	string = "% this document was created with M2TeX\n";
	string = string <> M2TeXToString[item[[1]]];
	
	(* add the header*)
	string = string <> "\n\n% packages and macros\n";
	Do[
		string = string <> M2TeXToString[pre];
		string = string <> "\n";
	,{pre, item[[2]]}];
	
	(* add the document *)
	string = string <> "\n% document\n";
	string = string <> M2TeXToString[item[[3]]];
	
	(* Return string of item *)
	string
];

(* Get document string*)
M2TeXDocumentToString[] := M2TeXToString[m2texDocument];


(* Add to document tree *)
M2TeXAppendToDocument[item_] := Module[
	{fullIndex},
	
	(* Get sequence for parts *)
	fullIndex = Table[
		{index, 2}
	,{index, m2texCurrentEnvironment}];
	fullIndex = fullIndex/.List->Sequence;
	
	(* Append new item *)
	AppendTo[m2texDocument[[fullIndex]], item];
	
	(* Check if new item is environment -> advance index list*)
	If[ MatchQ[item, _M2TeXEnvironment],
		AppendTo[m2texCurrentEnvironment, Length[ m2texDocument[[fullIndex]] ] ]
	];
];

(* Close current environment *)
M2TeXCloseEnvironment[] := (
	(* Check if the root item is not closed *)
	If[Length[m2texCurrentEnvironment] == 1,
		Print["Root Environment can not be closed!"],
		m2texCurrentEnvironment = Delete[m2texCurrentEnvironment, Length[m2texCurrentEnvironment]];
	];
);


(* add to preamble *)
M2TexAddPreamble[item_M2TeXNone] := None;
M2TexAddPreamble[item_] := Module[
	{},
	
	AppendTo[m2texDocument[[2]], item];
	
];




(** M2TeXToString for general data types **)
M2TeXToString[item_] := ToString[item];
M2TeXToString[None] := "";



(** Options structure **)

(* To text function for options (the list overload is different ) *)
M2TeXToStringOptions[item_] := M2TeXToString[item];
M2TeXToStringOptions[item_List] := Module[
	{string = ""},
	
	(* Loop over items *)
	Do[
		string = string <> M2TeXToStringOptions[ item[[i]] ] <> If[i == Length[item], "", ", "];
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

(* Overload constructor *)
M2TeXSetOptional[options_M2TeXNone] := M2TeXNone[];
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
	"Header" -> M2TeXNone[]
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
M2TeXSetPackage[packageName_, options_:M2TeXNone[]] := M2TeXSetCommand["usepackage", "Parameter" -> packageName, "OptionalParameter" -> options];


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
	"Header" -> M2TeXNone[],
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
		string = string <> "\n";
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









(** Plots **)

(* General Plot Item *)
Options[M2TeXSetPlot] = {
	"AddPlot" -> False
};
M2TeXSetPlot[table_, par_:M2TeXNone[], OptionsPattern[]] := Module[
	{
		temp
	},
	
	temp = M2TeXSetCommand[
		If[ OptionValue["AddPlot"], "addplot+", "addplot"],
		"OptionalParameter" -> par
	];
	
	M2TeXPlot[temp, table]
]
M2TeXToString[item_M2TeXPlot]:=Module[
	{string},
	
	string = M2TeXToString[item[[1]]];
	string = string <> " coordinates{%\n";
	Do[
		string = string <> "(" <> ToString[N[itemCoord[[1]]],CForm] <> "," <> ToString[N[itemCoord[[2]]],CForm] <> ")%\n";
	,{ itemCoord, item[[2]] }];
	string = string <> "};%";
	
	(* return string *)
	string
]











(** Define default document environments **)
M2TeXSetDocument["tikz"] := Module[{},
	(* define document class *)
	M2TeXSetDocument[
		"DocumentClass" -> M2TeXSetCommand[
			"documentclass",
			"ParameterList" -> {
				M2TeXSetOptionalOption["class=scrartcl"],
				M2TeXSetOption["standalone"]
			}
		]
	];
	
	(* add to header *)
	M2TexAddPreamble[M2TeXSetPackage["fontenc", "T1"]];
	M2TexAddPreamble[M2TeXSetPackage["inputenc", "utf8"]];
	M2TexAddPreamble[M2TeXSetPackage["amsmath"]];
	M2TexAddPreamble[M2TeXSetPackage["tikz"]];
	M2TexAddPreamble[M2TeXSetPackage["pgfplots"]];
	M2TexAddPreamble[M2TeXSetCommand[
		"pgfplotsset",
		"Parameter" -> "compat=newest,tick label style={font=\\footnotesize}"
		]];
];

















(** Create the pdf file **)
Options[M2TeXGeneratePdf] = {
	"ShowLog" -> False,
	"CleanTex" -> True
};
M2TeXGeneratePdf[name_,OptionsPattern[]] := Module[
	{
		texFile,
		pdfFile,
		out,
		extensions = {".log",".aux",".bbl",".blg"}
	},
	
	(* set the filenames *)
	texFile = FileNameJoin[{ Directory[], name <> ".tex" }];
	pdfFile = FileNameJoin[{ Directory[], name <> ".pdf" }]; 
	
	(* Save the string to *.tex file *)
	Export[ texFile, M2TeXDocumentToString[], "Text" ];
	
	(* Execute the LaTeX command*)
	out = Run["pdflatex --interaction=nonstopmode " <> texFile ];
	
	(* Check if process was sucsessfull *)
	If[ out != 0, Print["Error in LaTeX"]; ];

	(* Print output *)
	If[ OptionValue["ShowLog"], FilePrint[name<>".log"]];

	(* Delte auxilliary files *)
	If[ OptionValue["CleanTex"],
		extensions = AppendTo[extensions,".tex"]
	];
	DeleteFile[MapThread[StringJoin,{Table[name,Length[extensions]],extensions}]];
	
	(* Convert the pdf to png in the temporary directory *)
	gs = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64c.exe";
	RunProcess[{gs, "-sDEVICE=pngalpha", "-dTextAlphaBits=4", "-r360", "-o", "C:\\Users\\Steinbrecher\\AppData\\Local\\Temp\\plot-%03d.png",
		pdfFile
		}];
	
	(* Load the pdf file *)
	Import["C:\\Users\\Steinbrecher\\AppData\\Local\\Temp\\plot-001.png"]
];


*)



(********* ToString functions *********)
(*** Default ***)
M2TeXToString[item_] := ToString[item];
M2TeXToString[None] := "";

(*** For options ***)
M2TeXToStringOptions[item_] := M2TeXToString[item];
M2TeXToStringOptions[item_List] := Module[
	{string = ""},
	
	(* Loop over items *)
	Do[
		string = string <> M2TeXToStringOptions[ item[[i]] ] <> If[i == Length[item], "", ", "];
	,{i, Length[item]}];
	
	(* Return string *)
	string
];




(********* General options item *********)
(*** Definition of item ***)
Options[M2TeXOption] = {
	"OpenCloseCharacter" -> {"{", "}"}
};
M2TeXOption[options_, OptionsPattern[]] := M2TOption[<|
	"Options" -> options,
	"OpenCloseCharacter" -> OptionValue["OpenCloseCharacter"]
	|>
];

(*** ToString function ***)
M2TeXToString[item_M2TOption] := Module[{data},
	data = item[[1]];
	(* return string *)
	data[["OpenCloseCharacter",1]] <> M2TeXToStringOptions[data[["Options"]]] <> data[["OpenCloseCharacter",2]]
];

(*** Overload constructor ***)
M2TeXOption[None, OptionsPattern[]] := None;
M2TeXOptionOptional[options_] := M2TeXOption[options, "OpenCloseCharacter" -> { "[", "]" }];




(********* General options list item *********)
(*** Definition of item ***)
M2TeXOptionList[list_] := M2TOptionList[list];
M2TeXOptionList[None] := None;

(*** Overload the ToString function ***)
M2TeXToString[item_M2TOptionList] := Module[
	{string = ""},
	
	(* option list *)
	Do[
		string = string <> M2TeXToString[par];
	,{par, item[[1]]}];
	
	(* Return string of item *)
	string
];




(********* General command item *********)
(*** Definition of item ***)
Options[M2TeXCommand] = {
	"Parameter" -> None,
	"ParameterOptional" -> None,
	"ParameterList" -> None,
	"StartCharacter" -> "\\",
	"EndCharacter" -> "",
	"Header" -> None
};
M2TeXCommand[name_, OptionsPattern[]] := (
	M2TexAddPreamble[OptionValue["Header"]];
	M2TCommand[<|
		"Name" -> name,
		"Parameter" -> M2TeXOption[OptionValue["Parameter"]],
		"ParameterOptional" -> M2TeXOptionOptional[OptionValue["ParameterOptional"]],
		"ParameterList" -> M2TeXOptionList[OptionValue["ParameterList"]],
		"StartCharacter" -> OptionValue["StartCharacter"],
		"EndCharacter" -> OptionValue["EndCharacter"]
	|>]
)

(*** ToString function ***)
M2TeXToString[item_M2TCommand] := Module[{data, string},
	data = item[[1]];
	
	(* Command itself *)
	string = data["StartCharacter"] <> data["Name"];
	
	(* Add the options, if a parameter list exits, this one will be added *)
	If[ !( data["ParameterList"] === None ),
		(* Parameter list *)
		string = string <> M2TeXToString[ data["ParameterList"] ];
		,
		(* Parameters *)
		string = string <> M2TeXToString[ data["ParameterOptional"] ];
		string = string <> M2TeXToString[ data["Parameter"] ];
	];
	
	(* return string *)
	string <> data[ "EndCharacter" ]
];

(*** Overload constructor ***)
M2TeXCommand[name_, parameter_, parameterOptional_:None] := Module[{},
	M2TeXCommand[
		name,
		"Parameter" -> parameter,
		"ParameterOptional" -> parameterOptional
	]
];
M2TeXCommand[name_, Null, parameterOptional_] := M2TeXCommand[name, None, parameterOptional];

(*** For packages in the header ***)
M2TeXPackage[packageName_, options_:None] := M2TeXCommand["usepackage", packageName, options];




(********* General environment item *********)
(*** Definition of item ***)
Options[M2TeXEnvironment] = {
	"ParameterList" -> None,
	"Header" -> None
};
M2TeXEnvironment[name_, OptionsPattern[]] := Module[{},
	M2TexAddPreamble[OptionValue["Header"]];
	M2TEnvironment[<|
		"Name" -> name,
		"StartCommand" -> M2TeXCommand["begin", name],
		"EndCommand" -> M2TeXCommand["end", name],
		"OptionList" -> M2TeXOptionList[OptionValue["ParameterList"]],
		"Content" -> {}
	|>]		
]

(*** ToString function ***)
M2TeXToString[item_M2TEnvironment] := Module[
	{string, data},
	data = item[[1]];
	
	(* start the environment *)
	string = M2TeXToString[data["StartCommand"]];
	
	(* add the parameters *)
	string = string <> M2TeXToString[data["OptionList"]];
	
	(* write new line *)
	string = string <> "\n";
	
	(* Write the contents *)
	Do[
		string = string <> M2TeXToString[content];
		string = string <> "\n";
	,{ content, data["Content"] }];
	
	(* end the environment*)
	string = string <> M2TeXToString[data["EndCommand"]];

	(* Return string of item *)
	string
];

(*** Overload constructors ***)
M2TeXEnvironment[name_, optionList_] := M2TeXEnvironment[name, "ParameterList" -> optionList]







































(********* Packages dictionary *********)
ClearAll[f];
SetAttributes[f,HoldFirst];
f[x_]:= x = x + 5;

a = 5;
f[a];
a

(* 10 *)

f[a];
a

(* 15 *)













(* End private *)
End[];

EndPackage[];
