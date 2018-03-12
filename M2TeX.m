(*This is a package that enables mathematica to create *.tex files and create the Latex output.*)
BeginPackage["M2TeX`"];

(*Definition of Public Functions and Parameters*)

M2TeXToString::usage="TODO";

M2TeXText::usage="TODO";
M2TeXEmpty::usage="TODO";

M2TeXList::usage="TODO";

M2TeXOption::usage="TODO";
M2TeXOptionOptional::usage="TODO";
M2TeXOptionList::usage="TODO";

M2TeXCommand::usage="TODO";
M2TeXPackage::usage="TODO";
M2TeXEnvironment::usage="TODO";

M2TeXAddToEnvironment::usage="TODO";
M2TeXCloseActiveEnvironment::usage="TODO";
M2TeXCloseAll::usage="TODO";

M2TeXGetGlobal::usage="TODO";
M2TeXSetGlobal::usage="TODO";
M2TeXToStringGlobal::usage="TODO";

M2TeXDocument::usage="TODO";
M2TeXTemplates::usage="TODO";

M2TeXTikZ::usage="TODO";
M2TeXTikZCommand::usage="TODO";
M2TeXTikZAxis::usage="TODO";
M2TeXTikZPlot::usage="TODO";

M2TeXGeneratePDF::usage="TODO";

M2TeXPlotToPoints::usage="TODO";


(*Private Functions*)
Begin["`Private`"];



(********* Helper and compatibility functions *********)

(*True if file exists and is not a directory*)
fileQ[file_] := FileType[file] === File




(********* ToString functions *********)
(*** Default ***)
M2TeXToString[item_] := ToString[item];
M2TeXToString[None] := "";




(********* Text item *********)
(*** Default ***)
Options[M2TeXText] = {
	"Header" -> {},
	"NewLineCharacter" -> "\n"
};
M2TeXText[text_, OptionsPattern[]] := M2TText[<|"Text" -> text, "Header" -> OptionValue["Header"], "NewLineCharacter" -> ""|>];
M2TeXToString[M2TText[data_]] := data["Text"];




(********* Empty item, mainly for headers *********)
(*** Default ***)
M2TeXEmpty[header_] := M2TeXText["", "Header" -> header, "NewLineCharacter" -> ""];




(********* Get newline character if item has the option *********)
M2TeXGetNewLine[___] := "\n";
M2TeXGetNewLine[head_[dic_Association,optional___]] := If[KeyExistsQ[dic, "NewLineCharacter"],
	dic["NewLineCharacter"],
	M2TeXGetNewLine[]
];




(********* General list item *********)
(*** Definition of item ***)
Options[M2TeXList] = {
	"OpenCloseCharacter" -> {"{", "}"},
	"Seperator" -> ", ",
	"NewLineCharacter" -> "\n"
};
M2TeXList[data_, OptionsPattern[]] := M2TList[<|
	"Data" -> data,
	"OpenCloseCharacter" -> OptionValue["OpenCloseCharacter"],
	"Seperator" -> OptionValue["Seperator"],
	"NewLineCharacter" -> OptionValue["NewLineCharacter"]
	|>
];

(*** ToString function ***)
M2TeXToString[M2TList[data_]] := Module[
	{
		string = "",
		list
	},
	
	string = data[["OpenCloseCharacter",1]];
	
	(* add contents *)
	list = data[["Data"]];
	Do[
		string = string <> M2TeXToString[ list[[i]] ] <> If[i == Length[list], "", data["Seperator"]];
	,{i, Length[list]}];
	
	string = string <> data[["OpenCloseCharacter",2]];

	string
];

(*** overloads ***)
M2TeXList[None, OptionsPattern[]] := None;
M2TeXList[{None}, OptionsPattern[]] := None;




(********* General options item *********)
(*** Definition of item ***)
M2TeXOption[options_] := M2TeXList[Flatten[{options}], "OpenCloseCharacter" -> { "{", "}" }, "Seperator" -> ", "];
M2TeXOptionOptional[options_] := M2TeXList[Flatten[{options}], "OpenCloseCharacter" -> { "[", "]" }, "Seperator" -> ", "];




(********* General options list item *********)
(*** Definition of item ***)
M2TeXOptionList[list_] := M2TeXList[list, "Seperator" -> "", "OpenCloseCharacter" -> {"", ""}];
M2TeXOptionList[None] := None;




(********* General command item *********)
(*** Definition of item ***)
Options[M2TeXCommand] = {
	"Parameter" -> None,
	"ParameterOptional" -> None,
	"ParameterList" -> None,
	"StartCharacter" -> "\\",
	"EndCharacter" -> "",
	"NewLineCharacter" -> "\n",
	"Header" -> {}
};
M2TeXCommand[name_, OptionsPattern[]] := (
	M2TexAddPreamble[OptionValue["Header"]];
	M2TCommand[<|
		"Name" -> name,
		"Parameter" -> M2TeXOption[OptionValue["Parameter"]],
		"ParameterOptional" -> M2TeXOptionOptional[OptionValue["ParameterOptional"]],
		"ParameterList" -> M2TeXOptionList[OptionValue["ParameterList"]],
		"StartCharacter" -> OptionValue["StartCharacter"],
		"EndCharacter" -> OptionValue["EndCharacter"],
		"Header" -> OptionValue["Header"],
		"NewLineCharacter" -> OptionValue["NewLineCharacter"]
	|>]
)

(*** ToString function ***)
M2TeXToString[M2TCommand[data_]] := Module[{string},
	
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
	"StartCommand" -> None,
	"EndCommand" -> None,
	"Header" -> {},
	"NewLineCharacter" -> "\n"
};
M2TeXEnvironment[name_, OptionsPattern[]] := Module[{},
	M2TexAddPreamble[OptionValue["Header"]];
	M2TEnvironment[<|
		"Name" -> name,
		"StartCommand" -> If[
			OptionValue["StartCommand"] === None,
			M2TeXCommand["begin", name],
			OptionValue["StartCommand"]
		],
		"EndCommand" -> If[
			OptionValue["EndCommand"] === None,
			M2TeXCommand["end", name],
			OptionValue["EndCommand"]
		],
		"OptionList" -> M2TeXOptionList[OptionValue["ParameterList"]],
		"Content" -> {},
		"ActiveContent" -> Sequence[1, Key["Content"]],
		"Header" -> OptionValue["Header"],
		"NewLineCharacter" -> OptionValue["NewLineCharacter"]
	|>]
]

(*** ToString function ***)
M2TeXToString[M2TEnvironment[data_]] := Module[
	{string},
	
	(* start the environment *)
	string = M2TeXToString[data["StartCommand"]];
	
	(* add the parameters *)
	string = string <> M2TeXToString[data["OptionList"]];
	string = string <> M2TeXGetNewLine[data["StartCommand"]];
	
	(* Write the contents *)
	Do[
		string = string <> M2TeXToString[cont];
		string = string <> M2TeXGetNewLine[cont];
	,{cont, data["Content"]}];
	
	(* end the environment*)
	string = string <> M2TeXToString[data["EndCommand"]];

	(* Return string of item *)
	string
];

(*** Overload constructors ***)
M2TeXEnvironment[name_, optionList_] := M2TeXEnvironment[name, "ParameterList" -> optionList]




(********* Handle environment tree *********)
(*** Add to environment ***)
ClearAll[M2TeXAddToEnvironment];
SetAttributes[M2TeXAddToEnvironment, HoldFirst];
M2TeXAddToEnvironment[parent_, content_] := Module[
	{seq},
	
	(* get sequence to active content *)
	seq = parent[[ 1, Key["ActiveContent"] ]];
	
	(* Append to data *)
	AppendTo[parent[[ seq ]], content ];
	
	(* Add to active content if content was environment *)
	If[ MatchQ[content, _M2TEnvironment],
		seq = Sequence[seq, Length[parent[[ seq ]] ], 1, Key["Content"] ];
		parent[[1, Key["ActiveContent"] ]] = seq;
	];
];

(*** Close active environment ***)
ClearAll[M2TeXCloseActiveEnvironment];
SetAttributes[M2TeXCloseActiveEnvironment, HoldFirst];
M2TeXCloseActiveEnvironment[parent_] := Module[
	{seq, seqList},
	
	(* get sequence to active content *)
	seq = parent[[ 1, Key["ActiveContent"] ]];
	seqList = List[seq];
	
	(* Check if the sequence is long enough *)
	If[ Floor[Length[seqList] / 3] == 0,
		(* Can not close if there is only 1 environment active *)
		Print["Error, depth not long enough!"];,
		(* Remove the last 3 indices of sequence *)
		seqList = Delete[seqList, {{-1}, {-2}, {-3}}];
		parent[[ 1, Key["ActiveContent"] ]] = seqList /. List -> Sequence;
	];
];

(*** Close all child environments ***)
ClearAll[M2TeXCloseAll];
SetAttributes[M2TeXCloseAll, HoldFirst];
M2TeXCloseAll[parent_] := Module[{}, parent[[1, Key["ActiveContent"] ]] = Sequence[1, Key["Content"]]; ];

(*** Overloads for document ***)
M2TeXAddToEnvironment[content_] := M2TeXAddToEnvironment[M2Tdocument, content]; 
M2TeXCloseActiveEnvironment[] := M2TeXCloseActiveEnvironment[M2Tdocument];




(********* Global document items *********)
M2TeXToStringGlobal[] := M2TeXToString[M2Tdocument];
M2TeXGetGlobal[] := M2Tdocument;
M2TeXSetGlobal[item_] := Module[{}, M2Tdocument = item;];
M2TeXSetGlobal[template_String] := Module[{}, M2Tdocument = M2TeXTemplates[template];];




(********* Header Handle *********)
(*** Add header of item to list ***)
ClearAll[M2TeXGetHeader];
SetAttributes[M2TeXGetHeader, HoldFirst];

M2TeXGetHeader[headerList_, _] := None;

(*** Get header from environments ***)
M2TeXGetHeader[headerList_, M2TEnvironment[data_]] := (
	(* Add own header *)
	M2TeXGetHeader[headerList, _[ data ] ];
	
	(* Add header of children *)
	Do[
		M2TeXGetHeader[headerList, item];
	,{item, data["Content"]}];
);

(*** Header for general items ***)
M2TeXGetHeader[headerList_, fun_[dic_Association]] := If[
	(* Only do something if the dic has a header item *)
	KeyExistsQ[dic, "Header"],
	(* check if item exists already *)
	Do[
		If[! MemberQ[Hash /@ headerList, Hash@item],
			AppendTo[headerList, item];
		];
	,{item, Flatten[{dic["Header"]}]  }];
];




(********* Document handle *********)
(*** Document item ***)
M2TeXDocument[documentClass_, preamble_:{}] := Module[
	{tempEnvironment, data},
	
	(* Get an environment, add data and turn it to an document *)
	tempEnvironment = M2TeXEnvironment["document"];
	data = tempEnvironment[[ 1 ]];
	
	(* Add the preamble and document class *)
	data["Header"] = preamble;
	AppendTo[data, "DocumentClass" -> documentClass];
	
	(* Return document item *)
	M2TDocument[data]
];

(*** ToString function ***)
M2TeXToString[M2TDocument[data_]] := Module[
	{string, headerList={}},
	
	string = M2TeXToString[data["DocumentClass"]];
	string = string <> "\n";
	string = string <> "\n";
	
	(* get header List*)
	M2TeXGetHeader[headerList, M2TEnvironment[data]];
	
	(* add the headers to the string *)
	string = string <> M2TeXToString[
		M2TeXList[headerList, "Seperator" -> "\n", "OpenCloseCharacter" -> {"", ""}]
	];
	
	string = string <> "\n";
	string = string <> "\n";
	string = string <> M2TeXToString[ M2TEnvironment[data] ];
	string
];




(********* Document templates *********)
(*** Article template ***)
M2TeXTemplates["article"] := Module[
	{ documentClass, preamble },
	
	documentClass = M2TeXCommand["documentclass", "scrartcl"];
	preamble = {
		M2TeXPackage["fontenc", "T1"],
		M2TeXPackage["inputenc", "utf8"],
		M2TeXPackage["amsmath"]
	};
	
	M2TeXDocument[documentClass, preamble]
];

(*** standalone template ***)
M2TeXTemplates["standalone"] := Module[
	{ documentClass, preamble },
	
	documentClass = M2TeXCommand["documentclass", "standalone", "class=scrartcl"];
	preamble = {
		M2TeXPackage["fontenc", "T1"],
		M2TeXPackage["inputenc", "utf8"],
		M2TeXPackage["amsmath"],
		M2TeXPackage["tikz"],
		M2TeXPackage["pgfplots"],
		M2TeXCommand["pgfplotsset", "compat=newest,tick label style={font=\\footnotesize}"]
	};
	
	M2TeXDocument[documentClass, preamble]
];




(********* Tikz tools *********)
(*** General tikz command ***)
M2TeXTikZCommand[name_] := M2TeXTikZCommand[name, "", None]
M2TeXTikZCommand[name_, text_] := M2TeXTikZCommand[name, text, None]
M2TeXTikZCommand[name_, Null, optionList_, options___] := M2TeXTikZCommand[name, "", optionList, options]
M2TeXTikZCommand[name_, text_, optionList_, options___] := Module[{tempData},
	
	tempData = M2TeXCommand[name, "ParameterOptional" -> optionList, "EndCharacter" -> ";", options][[1]];
	AppendTo[tempData, "Text" -> text];
	
	M2TTikZCommand[tempData]
];
M2TeXToString[M2TTikZCommand[data_]] := Module[{string, dataTemp},
	
	(*Get string from command, without the end character *)
	dataTemp = data;
	dataTemp["EndCharacter"] = "";
	string = M2TeXToString[M2TCommand[dataTemp]];
	If [ data["Text"] != "",
		string = string <> " ";
		string = string <> data["Text"];
	];
	string = string <> data["EndCharacter"];
	
	(* return string *)
	string
];

(*** Tikz picture ***)
M2TeXTikZ[] := M2TeXEnvironment["tikzpicture"];

(*** Axis for pgf plot ***)
M2TeXTikZAxis[] := M2TeXTikZAxis[None];
M2TeXTikZAxis[list_] := M2TeXEnvironment["axis", "ParameterList" -> {M2TeXOptionOptional[list]}]

(*** Plot datapoints ***)
Options[M2TeXTikZPlot] = {
	"AddPlot" -> False,
	"Header" -> {},
	"PostText" -> ""
};
M2TeXTikZPlot[table_, options___Rule] := M2TeXTikZPlot[table, None, options]
M2TeXTikZPlot[table_, par_, OptionsPattern[]] := Module[{tempData},
	(* Get a command*)
	tempData = If[
		OptionValue["AddPlot"],
		M2TeXTikZCommand["addplot+", Null, par, "Header" -> OptionValue["Header"] ],
		M2TeXTikZCommand["addplot", Null, par, "Header" -> OptionValue["Header"]]
	][[1]];
	
	(* Add table data *)
	(* TODO: add check to table dimmensions here *)
	AppendTo[tempData, "Table" -> table];
	
	(* Add PostText *)
	AppendTo[tempData, "PostText" -> OptionValue["PostText"]];
	
	(* return plot command *)
	M2TTikZPlot[tempData]
];
M2TeXToString[M2TTikZPlot[data_]] := Module[{string, dataTemp, tempString},
	
	(* Get the coordinates *)
	string = "coordinates{%\n";
	Do[
		(* Check if both values are nummeric, otherwise skip the row *)
		If[ MemberQ[NumericQ /@ coord, False],
			(* coord is not numeric *)
			Print["Non numeric value!"],
			string = string <> "(" <> ToString@CForm[N[coord[[1]] ]] <> ", " <> ToString@CForm[N[coord[[2]] ]] <> ")%\n";
		];
	,{coord, data["Table"]}];
	string = string <> "}";
	
	(* check if there is a PostText *)
	tempString = M2TeXToString[data["PostText"]];
	If[
		tempString != "",
		string = string <> " " <> tempString;
	];
	
	(* Add the command to string *)
	dataTemp = data;
	AppendTo[dataTemp, "Text" -> string];
	M2TeXToString[M2TTikZCommand[dataTemp]]
];




(********* Compile the document *********)
(*** Create the pdf file ***)
Options[M2TeXGeneratePDF] = {
	"SaveTeX" -> False,
	"OutputPDF" -> False
};
M2TeXGeneratePDF[name_, options___Rule] := M2TeXGeneratePDF[name, M2Tdocument, options];
M2TeXGeneratePDF[name_, environment_, OptionsPattern[]] := Module[
	{
		nameTemp,
		texFileTemp,
		pdfFileTemp,
		texFile,
		pdfFile,
		out,
		outImage = {}
	},
	
	(* The files will be compiled in the temp directory *)
	nameTemp = "M2TeX_" <> ToString[ $KernelID ];
	texFileTemp = FileNameJoin[{ $TemporaryDirectory, nameTemp <> ".tex" }];
	pdfFileTemp = FileNameJoin[{ $TemporaryDirectory, nameTemp <> ".pdf" }]; 
	texFile = FileNameJoin[{ Directory[], name <> ".tex" }];
	pdfFile = FileNameJoin[{ Directory[], name <> ".pdf" }];
	
	(* Save the string to *.tex file *)
	Export[ texFileTemp, M2TeXToString[environment], "Text" ];
	
	(* Move the tex file *)
	If[ OptionValue["SaveTeX"],
		SaveOverwrite[texFileTemp, texFile];
	];
	
	(* Execute the LaTeX command*)
	If[FileExistsQ[pdfFileTemp], DeleteFile[pdfFileTemp];];
	out = RunProcess[{"pdflatex", "-halt-on-error", "--interaction=nonstopmode", texFileTemp}, ProcessDirectory -> $TemporaryDirectory];
	
	(* Check if there were tex errors *)
	If[ texErrorQ[out["StandardOutput"]],
		Print["LaTeX Error!"];
		,
		(* Move the files *)
		SaveOverwrite[pdfFileTemp, pdfFile];
		
		(* Get the results *)
		If[ OptionValue["OutputPDF"],
			(* Load the pdf file *)
			outImage = Import[pdfFileTemp];
			,
	
			(* Delete all image files in temp directory *)
			DeleteFile[FileNames[nameTemp <> "_*.png", $TemporaryDirectory]];
	
			(* Convert the pdf into png files *)
			gs = "C:\\Program Files\\gs\\gs9.22\\bin\\gswin64c.exe";
			RunProcess[{gs, "-sDEVICE=pngalpha", "-dTextAlphaBits=4", "-r360", "-o", nameTemp <> "_%03d.png", pdfFileTemp}, ProcessDirectory -> $TemporaryDirectory];
			
			(* Import png images *)
			outImage = Import /@ FileNames[ nameTemp <> "_*.png", $TemporaryDirectory];
		];
	];
	
	(* Return the results *)
	{
		outImage,
		getErrorWarningOverfull[ out[["StandardOutput"]] ]
	}
];




(********* Utility functions *********)
(*** Save file, and delete existing file bevore that ***)
SaveOverwrite[source_, destination_] := Module[{},
	If[ FileExistsQ[destination],
		DeleteFile[destination];
	];
	CopyFile[source, destination];
];




(********* Get error code from latex output *********)
(*** Functions catch errors/warnings/overfull ***)
errorQ[text_String] := StringMatchQ[text, StartOfString ~~ "!" ~~ __]
warningQ[text_String] := StringMatchQ[text, StartOfString ~~ "LaTeX Warning" ~~ __]
overfullQ[text_String] := StringMatchQ[text, StartOfString ~~ "Overfull" ~~ __]

(*** Get the notifications from string ***)
getErrorWarningOverfull[string_String] := Module[
	{
		stringLines
	},
	
	(* Split the string at the new line markers *)
	stringLines = StringSplit[string, {"\r\n"}];
	
	(* return the messages *)
	{
		gerErrorString[stringLines, errorQ, 3],
		gerErrorString[stringLines, warningQ, 1],
		gerErrorString[stringLines, overfullQ, 1]
	}
];

(*** Get a string from the error code ***)
gerErrorString[lines_, checkQ_, maxLines_] := Module[
	{
		positions,
		indicesList
	},

	(* Positions of error *)
	positions = Position[checkQ /@ lines, True] // Flatten;
	
	(* Indices of list to join *)
	indicesList = Table[
		Table[
			j
		,{j,i,Min[{i+maxLines-1, Length[lines]}]}]
	,{i, positions}];
	
	(* Join the error codes together *)
	Table[
		StringJoin[
			Insert[lines[[indices]], "\n", Table[{i}, {i, 2, Length[lines[[indices]]]}]]
		]
	,{indices, indicesList}]
];



(********* Functions from MaTeX *********)
(* This function is used to try to detect errors based on the log file.
   It is necessary because on Windows RunProcess doesn't capture the correct exit code. *)
texErrorQ[log_String] := Count[StringSplit[StringDelete[log, "\r"], "\n"], line_ /; StringMatchQ[line, errorLinePattern]] > 0
errorLinePattern = ("! "~~___) | ("!"~~___~~"error"~~___) | (___~~"Fatal error occurred"~~___);





(* End private *)
End[];

EndPackage[];
