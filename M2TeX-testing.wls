#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*M2TeX testing*)


testingDir=FileNameJoin[{NotebookDirectory[], "testing_temp"}];
If[DirectoryQ[testingDir],DeleteDirectory[testingDir, DeleteContents->True]];
CreateDirectory[testingDir];


SetDirectory[testingDir];
<<M2TeX`


(* ::Section:: *)
(*Testing functions*)


tests={};
testM2Tex[var1_,var2_]:=(tests=Append[tests,VerificationTest[var1,var2]];)


(* ::Section:: *)
(*Test cases*)


(* ::Subsection:: *)
(*Text*)


testM2Tex[
M2TeXText[
"stuff",
"Header"->{},
"NewLineCharacter"->"\n"
]//M2TeXToString,
"stuff"
]


(* ::Subsection:: *)
(*List*)


testM2Tex[
M2TeXList[
{"value","value2"},
"Seperator"->"<>",
"OpenCloseCharacter"->{"<Open>","<Close>"}
]//M2TeXToString,
"<Open>value<>value2<Close>"
]


testM2Tex[
M2TeXList[None]//M2TeXToString,
""
]


(* ::Subsection:: *)
(*Options*)


testM2Tex[
M2TeXOption["value"]//M2TeXToString,
"{value}"
]


testM2Tex[
M2TeXOption[{"value",{"op2",{"op3","op4"}}}]//M2TeXToString,
"{value, op2, op3, op4}"
]


testM2Tex[
M2TeXOptionOptional[{"optional1","optional2"}]//M2TeXToString,
"[optional1, optional2]"
]


(* ::Subsection:: *)
(*Commands*)


testM2Tex[
M2TeXCommand[
"dostuff",
"Parameter"->"val1",
"ParameterOptional"->"val2",
"ParameterList"->None,
"StartCharacter"->"<\\>",
"EndCharacter"->"</>"
]//M2TeXToString,
"<\\>dostuff[val2]{val1}</>"
]


testM2Tex[
M2TeXCommand[
"dostuff",
"ParameterList"->{
M2TeXOption[{"op1","op11"}],
M2TeXOptionOptional[{"op2","op22"}],
M2TeXOption["op3"]
}
]//M2TeXToString,
"\\dostuff{op1, op11}[op2, op22]{op3}"
]


testM2Tex[
M2TeXCommand["dostuff"]//M2TeXToString,
"\\dostuff"
]


testM2Tex[
M2TeXCommand["dostuff","opt"]//M2TeXToString,
"\\dostuff{opt}"
]


testM2Tex[
M2TeXCommand["dostuff",{"opt","OPT"},"opt2"]//M2TeXToString,
"\\dostuff[opt2]{opt, OPT}"
]


testM2Tex[
M2TeXCommand["dostuff",,"opt2"]//M2TeXToString,
"\\dostuff[opt2]"
]


testM2Tex[
M2TeXPackage["name",{"opt1","opt2"}]//M2TeXToString,
"\\usepackage[opt1, opt2]{name}"
]


(* ::Subsection:: *)
(*Environments*)


testM2Tex[
M2TeXEnvironment[
"document",
"ParameterList"->{
M2TeXOption["op1"],
M2TeXOptionOptional["op2"]
},
"StartCommand"->"START",
"EndCommand"->"END"
]//M2TeXToString,
"START{op1}[op2]\nEND"
]


testM2Tex[
M2TeXEnvironment["document"]//M2TeXToString,
"\\begin{document}\n\\end{document}"
]


testM2Tex[
M2TeXEnvironment["document",{
M2TeXOption["op1"],
M2TeXOptionOptional["op2"]
}]//M2TeXToString,
"\\begin{document}{op1}[op2]\n\\end{document}"
]


doc=M2TeXEnvironment["document"];
M2TeXAddToEnvironment[doc,M2TeXCommand["section","Introduction"]]
M2TeXAddToEnvironment[doc,"Bla bla bla..."]
testM2Tex[
doc//M2TeXToString,
"\\begin{document}\n\\section{Introduction}\nBla bla bla...\n\\end{document}"
]


doc=M2TeXEnvironment["document"];
M2TeXAddToEnvironment[doc,M2TeXEnvironment["picture"]]
M2TeXAddToEnvironment[doc,"Bla bla bla..."]
testM2Tex[
doc//M2TeXToString,
"\\begin{document}\n\\begin{picture}\nBla bla bla...\n\\end{picture}\n\\end{document}"
]


doc=M2TeXEnvironment["document"];
M2TeXAddToEnvironment[doc,M2TeXEnvironment["picture"]]
M2TeXAddToEnvironment[doc,"picture"]
M2TeXCloseActiveEnvironment[doc]
M2TeXAddToEnvironment[doc,"Bla bla bla..."]
testM2Tex[
doc//M2TeXToString,
"\\begin{document}\n\\begin{picture}\npicture\n\\end{picture}\nBla bla bla...\n\\end{document}"
]


doc=M2TeXEnvironment["document"];
M2TeXAddToEnvironment[doc,M2TeXEnvironment["picture"]]
M2TeXAddToEnvironment[doc,"Text1"]
M2TeXCloseActiveEnvironment[doc]
M2TeXAddToEnvironment[doc,"Text2"]
M2TeXCloseActiveEnvironment[doc]
M2TeXAddToEnvironment[doc,"Text3"]
testM2Tex[
doc//M2TeXToString,
"\\begin{document}\n\\begin{picture}\nText1\n\\end{picture}\nText2\nText3\n\\end{document}"
]


doc=M2TeXEnvironment["document"];
M2TeXAddToEnvironment[doc,M2TeXEnvironment["picture"]]
M2TeXAddToEnvironment[doc,M2TeXEnvironment["pictureChild"]]
M2TeXCloseAll[doc]
M2TeXAddToEnvironment[doc,"content"]
testM2Tex[
doc//M2TeXToString,
"\\begin{document}\n\\begin{picture}\n\\begin{pictureChild}\n\\end{pictureChild}\n\\end{picture}\ncontent\n\\end{document}"
]


doc=M2TeXEnvironment["document"];
M2TeXAddToEnvironment[doc,M2TeXText["This is a command " ,"NewLineCharacter"->""]]
M2TeXAddToEnvironment[doc,M2TeXCommand["Text","NewLineCharacter"->""]]
M2TeXAddToEnvironment[doc," and this is in the same line"]
testM2Tex[
doc//M2TeXToString,
"\\begin{document}\nThis is a command \\Text and this is in the same line\n\\end{document}"
]


(* ::Subsection:: *)
(*Documents*)


doc=M2TeXDocument[
M2TeXCommand["documentclass","scrartcl","11pt"],
{
M2TeXPackage["amsmath"],
M2TeXPackage["MinionPro","lf"]
}
];
M2TeXAddToEnvironment[doc,"content"]
AppendTo[doc[[1, "Header"]],"% created with M2TeX"];
M2TeXAddToEnvironment[doc,M2TeXEmpty[{M2TeXPackage["package"]}]]
M2TeXAddToEnvironment[doc,M2TeXCommand["new","Header"->{M2TeXPackage["color"]}]]
M2TeXAddToEnvironment[doc,M2TeXCommand["new","Header"->{M2TeXPackage["color2"]}]]
M2TeXAddToEnvironment[doc,M2TeXCommand["new","Header"->{M2TeXPackage["color"]}]]

testM2Tex[
doc//M2TeXToString,
"\\documentclass[11pt]{scrartcl}\n\n\\usepackage{amsmath}\n\\usepackage[lf]{MinionPro}\n% created with M2TeX\n\\usepackage{package}\n\\usepackage{color}\n\\usepackage{color2}\n\n\\begin{document}\ncontent\n\\new\n\\new\n\\new\n\\end{document}"
]


(* ::Subsection:: *)
(*Document templates*)


(* ::Text:: *)
(*Following templates are available*)


testM2Tex[
M2TeXTemplates["article"]//M2TeXToString,
"\\documentclass{scrartcl}\n\n\\usepackage[T1]{fontenc}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\n\\begin{document}\n\\end{document}"
]


testM2Tex[
M2TeXTemplates["standalone"]//M2TeXToString,
"\\documentclass[class=scrartcl, border=0]{standalone}\n\n\\usepackage[T1]{fontenc}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\\usepackage{tikz}\n\\usepackage{pgfplots}\n\\pgfplotsset{compat=newest,tick label style={font=\\footnotesize}}\n\n\\begin{document}\n\\end{document}"
]


(* ::Subsection:: *)
(*TikZ tools*)


(* ::Text:: *)
(*Command for TikZ*)


testM2Tex[
M2TeXTikZCommand["node"]//M2TeXToString,
"\\node;"
]


testM2Tex[
M2TeXTikZCommand["node","at (0,0)"]//M2TeXToString,
"\\node at (0,0);"
]


testM2Tex[
M2TeXTikZCommand["node","at (0,0)","black"]//M2TeXToString,
"\\node[black] at (0,0);"
]


testM2Tex[
M2TeXTikZCommand["node",,"black"]//M2TeXToString,
"\\node[black];"
]


testM2Tex[
M2TeXTikZAxis["meins"]//M2TeXToString,
"\\begin{axis}[meins]\n\\end{axis}"
]


testM2Tex[
M2TeXTikZAxis["Log"->False]//M2TeXToString,
"\\begin{axis}\n\\end{axis}"
]


testM2Tex[
M2TeXTikZAxis["Log"->"x"]//M2TeXToString,
"\\begin{semilogxaxis}\n\\end{semilogxaxis}"
]


testM2Tex[
M2TeXTikZAxis["Log"->"y"]//M2TeXToString,
"\\begin{semilogyaxis}\n\\end{semilogyaxis}"
]


testM2Tex[
M2TeXTikZAxis["Log"->"xy"]//M2TeXToString,
"\\begin{loglogaxis}\n\\end{loglogaxis}"
]


testM2Tex[
M2TeXTikZAxis["Log"->"loglog"]//M2TeXToString,
"\\begin{loglogaxis}\n\\end{loglogaxis}"
]


testM2Tex[
M2TeXTikZPlot[{{1,3},{2,4}},{"black","white"}]//M2TeXToString,
"\\addplot[black, white] coordinates{%\n(1., 3.)%\n(2., 4.)%\n};"
]


testM2Tex[
M2TeXTikZPlot[{{1,3},{a,4}},{"black","white"},"AddPlot"->True]//M2TeXToString,
"\\addplot+[black, white] coordinates{%\n(1., 3.)%\n};"
]


testM2Tex[
M2TeXTikZPlot[{{1,3},{2,4}},{"black","white"},"PostText"->"node{$A$}"]//M2TeXToString,
"\\addplot[black, white] coordinates{%\n(1., 3.)%\n(2., 4.)%\n} node{$A$};"
]


table={{1,3},{2,4}};
tikz=M2TeXTikZ[];
M2TeXAddToEnvironment[tikz,M2TeXTikZAxis[]];
M2TeXAddToEnvironment[tikz,M2TeXTikZPlot[table]];
M2TeXAddToEnvironment[tikz,M2TeXTikZPlot[table]];

M2TeXTikZExportCode[tikz,"trig"];
testM2Tex[
Import["trig.tex","Text"],
"\\begin{tikzpicture}\n\\begin{axis}\n\\addplot table{trig_data_1.dat};\n\\addplot table{trig_data_2.dat};\n\\end{axis}\n\\end{tikzpicture}"
]
testM2Tex[
Import["trig_data_1.dat","Text"],
"1. 3.\n2. 4."
]
testM2Tex[
Import["trig_data_2.dat","Text"],
"1. 3.\n2. 4."
]


M2TeXTikZExportCode[tikz,"trig2","DataPath"->"data"];
testM2Tex[
Import["trig2.tex","Text"],
"\\begin{tikzpicture}\n\\begin{axis}\n\\addplot table{data/trig2_data_1.dat};\n\\addplot table{data/trig2_data_2.dat};\n\\end{axis}\n\\end{tikzpicture}"
]
testM2Tex[
Import["trig2_data_1.dat","Text"],
"1. 3.\n2. 4."
]
testM2Tex[
Import["trig2_data_2.dat","Text"],
"1. 3.\n2. 4."
]


(* ::Subsection:: *)
(*Global document structure*)


M2TeXSetGlobal[M2TeXEnvironment["doc"]]
M2TeXAddToEnvironment["text"]
testM2Tex[
M2TeXToStringGlobal[],
"\\begin{doc}\ntext\n\\end{doc}"
]


M2TeXSetGlobal["article"]
testM2Tex[
M2TeXToStringGlobal[],
"\\documentclass{scrartcl}\n\n\\usepackage[T1]{fontenc}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\n\\begin{document}\n\\end{document}"
]


M2TeXSetGlobal["article"]
global=M2TeXGetGlobal[];
global[[1, "Header"]]={"% comment in header"};
M2TeXSetGlobal[global];
testM2Tex[
M2TeXToStringGlobal[],
"\\documentclass{scrartcl}\n\n% comment in header\n\n\\begin{document}\n\\end{document}"
]


M2TeXSetGlobal["article"];
M2TeXAddToEnvironment[M2TeXEmpty[{"% comment in header","% also comment in header"}]];
testM2Tex[
M2TeXToStringGlobal[],
"\\documentclass{scrartcl}\n\n\\usepackage[T1]{fontenc}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n% comment in header\n% also comment in header\n\n\\begin{document}\n\\end{document}"
]


M2TeXSetGlobal["article"];
M2TeXAddToEnvironment[M2TeXEnvironment["picture1"]];
M2TeXAddToEnvironment[M2TeXEnvironment["picture2"]];
M2TeXCloseActiveEnvironment[];
M2TeXAddToEnvironment[M2TeXEnvironment["picture3"]];
M2TeXCloseAll[]
M2TeXAddToEnvironment[M2TeXEnvironment["picture4"]];

testM2Tex[
M2TeXToStringGlobal[],
"\\documentclass{scrartcl}\n\n\\usepackage[T1]{fontenc}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\n\\begin{document}\n\\begin{picture1}\n\\begin{picture2}\n\\end{picture2}\n\\begin{picture3}\n\\end{picture3}\n\\end{picture1}\n\\begin{picture4}\n\\end{picture4}\n\\end{document}"
]


(* ::Subsection:: *)
(*Get Lines from Plot*)


plt=Plot[{x^2,1/x},{x,-1,1},PlotRange->{{-1,1},{-4,4}},PlotPoints->4,MaxRecursion->1];
testM2Tex[
M2TeXPlotToPoints[plt],
{{{-0.9999993333333334`,0.9999986666671111`},{-0.6793982003750035`,0.46158191467279347`},{-0.3587970674166737`,0.1287353355868051`},{-0.01122870131251233`,0.00012608373316561611`},{-5.436844846330497`*^-18,0.0037766576354959455`},{0.33633966479164906`,0.11312437011215885`},{0.6681694990624912`,0.44645047947742045`},{0.9999993333333334`,0.9999986666671111`}},{{-0.9999993333333334`,-1.000000666667111`},{-0.6793982003750035`,-1.4718908578916396`},{-0.3587970674166737`,-2.7870907842139427`},{-0.3333333333333333`,-3.`}},{{0.3333333333333333`,3.`},{0.9999993333333334`,1.000000666667111`}}}
]


(* ::Section:: *)
(*Summary*)


TestReport[tests]
