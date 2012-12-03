(* ::Package:: *)

BeginPackage["CustomGraphFunctions`"];


GenerateTopology::usage = "GenerateTopology[g,w] generates a list whose elements encode edges in directed graph g with weights randomly  picked from list w.";
TopologyList::usage = "TopologyList[g,w] generates output of directed graph g with weights w suitable for export to use with CNetwork.";
EncodeGraph::usage = "EncodeGraph[] generates a directed graph from the ENCODE consortium.";
RandomIOGraph::usage = "RandomIOGraph[g,i] generates a random graph with the same In/Out degree distribution as graph g, by shuffeling random edges i times.";
RandomAllGraph::usage = "RandomAllGraph[g] generates a graph with the same degree distribution as graph g.";
HDegree::usage = "HDegree[g,v] returns the hierarchy degree of vertex v of graph g.";
HierarchyLevels::usage = "HierarchyLevels[g,n,s] gives a list of n hierarchy levels of graph g, by sshing to server s and executing a Matlab script.";
HierarchyHistogram::usage = "HierarchyHistogram[g,n,s] gives a histogram of n hierarchy levels of graph g, by sshing to server s and executing a Matlab script.";
LevelInteractions::usage = "LevelInteractions[g,n,s] gives a level interaction matrix of n hierarchy levels of graph g, by sshing to server s and executing a Matlab script.";
StartRun::usage = "StartRun[inputdirs,parameters,runname,server,nproc] starts a run on a remote server with the files in inputdirs as input.";
ResultsIndex::usage = "ResultsIndex[inputdir] searches the directory inputdir for result files and displays the available run results as a table.";
PrepareRun::usage = "PrepareRun[inputdirs,parameters,runname,nproc,outputdir] prepares a run in the outputdir with the files in inputdirs as input.";


Begin["`Private`"]

GenerateTopology[graph_Graph,weights_List]:=
	Module[{translationRules},(
		translationRules=Table[Rule[VertexList[graph][[i]],i],{i,1,VertexCount[graph]}];
		Table[
			Insert[
				Replace[EdgeList[graph][[i]]/.translationRules,DirectedEdge->List,\[Infinity],Heads->True]
				,RandomChoice[weights],2],
			{i,1,EdgeCount[graph]}
		]
	)]

TopologyList[graph_Graph,weights_List]:=
	Module[{output},(
		output=GenerateTopology[graph,weights];
		PrependTo[output,VertexCount[graph]]
	)]

EncodeGraph[] :=
	Module[{encodeData,tfs,encodeEdges},(
		encodeData=Import["/Users/jelmerderonde/Documents/Code/CNetwork/NETWORKS/ENCODE/enets2.Proximal_filtered.txt",{"Text","Words"}];
		tfs=Import["/Users/jelmerderonde/Documents/Code/CNetwork/NETWORKS/ENCODE/tfs.txt",{"Text","Words"}];
		encodeEdges=Cases[Partition[encodeData,2],{Apply[Alternatives,tfs],Apply[Alternatives,tfs]}];
		Graph[Apply[DirectedEdge,encodeEdges,2]]
	)]

RandomIOGraph[graph_Graph,i_Integer]:=
	Module[{newGraph,testEdges},(
		newGraph=graph;
		testEdges=Table[EdgeList[newGraph][[RandomInteger[EdgeCount[newGraph]]]],{2}];
		Table[
			If[
				Length[EdgeList[newGraph,testEdges[[2,1]]\[DirectedEdge]testEdges[[1,2]]]]==Length[EdgeList[newGraph,testEdges[[1,1]]\[DirectedEdge]testEdges[[2,2]]]]==0,
				
				newGraph=EdgeDelete[newGraph,testEdges];
				newGraph=EdgeAdd[newGraph,{testEdges[[2,1]]\[DirectedEdge]testEdges[[1,2]],testEdges[[1,1]]\[DirectedEdge]testEdges[[2,2]]}];
			];,
		{i}
		];
		newGraph
	)]

RandomAllGraph[graph_Graph] :=
	Module[{newGraph},(
		newGraph=Replace[EdgeList[RandomGraph[DegreeGraphDistribution[VertexDegree[graph]]]],UndirectedEdge->DirectedEdge,\[Infinity],Heads->True];
		Graph[newGraph]
	)]

HDegree[graph_Graph,v_]:=
	(VertexOutDegree[graph,v]-VertexInDegree[graph,v])/(VertexOutDegree[graph,v]+VertexInDegree[graph,v])

HierarchyLevels[graph_Graph,nlevels_Integer,server_String]:=
	Module[{initialDirectory,serverFolder,levels,tempDir},(
		tempDir=CreateDirectory[];
		initialDirectory=Directory[];
		serverFolder=server<>":Project/Main/Matlab/Automated/";
		
		SetDirectory[tempDir];
		
		Export["input.mat",AdjacencyMatrix[graph]];
		Run["scp "<>ToString[Directory[]]<>"/input.mat "<>serverFolder];

		Export["command.m",
			"s1=importdata('input.mat');
			ibot=[];
			itop=[];
			nlevels="<>ToString[nlevels]<>";
			[levels, n_fb, out]=hierarchy_levels(s1,nlevels,itop,ibot);
			save('output','levels');
			exit;"
		,"Text"];
		
		Run["scp "<>ToString[Directory[]]<>"/command.m "<>serverFolder];
		Run["ssh "<>server<>" \"cd "<>StringTrim[serverFolder,server<>":"]<>"; matlab -r command\""];
		Run["scp "<>serverFolder<>"output.mat "<>ToString[Directory[]]<>"/"];
		
		levels=IntegerPart/@Flatten[Import["output.mat"]];
		
		DeleteDirectory[tempDir,DeleteContents->True];
		SetDirectory[initialDirectory];
		
		levels
	)]

HierarchyHistogram[graph_Graph,nlevels_Integer,server_String]:=
	Module[{colors,levels,data,h,v,l},(
		colors={Green,Blue,Red,Purple,Orange,Cyan};
		levels=HierarchyLevels[graph,nlevels,server];
		data=Table[
			Cases[
				Table[
					If[VertexDegree[graph][[v]]>0,
						{HDegree[graph,VertexList[graph][[v]]],levels[[v]]}
					],
					{v,1,VertexCount[graph]}],
				{h_,l}->h
			],
			{l,Range[Max[levels]]}
		];
		
		Histogram[
			data,
			{-1.1+.2/3,1.1-.2/3,.2/3},
			ChartStyle->colors,
			ChartLayout->"Stacked",
			ChartLegends->ToString/@Range[Max[levels]]
		]
	)]

LevelInteractions[graph_Graph,nlevels_Integer,server_String]:=
	Module[{s,levels},(
		levels=HierarchyLevels[graph,nlevels,server];
		s=SparseArray[
			Apply[Rule,
				Apply[List,
					DeleteCases[Tally[EdgeList[graph]/.Table[Rule[VertexList[graph][[v]],levels[[v]]],{v,1,VertexCount[graph]}]],
			{0\[DirectedEdge]0,_}],2],1]];
		ArrayPlot[
			s,
			Mesh->True,
			Epilog->{Red,MapIndexed[Text[#1,Reverse[#2-1/2]]&,Reverse[s],{2}]}
		]
	)]

StartRun[inputdirs_List,parameters_List,runname_String,server_String,nproc_Integer]:=
	Module[{inputfiles,tempdir,initialdirectory,filestocopy,runs},(
		tempdir=CreateDirectory[];
		initialdirectory=Directory[];
		SetDirectory[tempdir];
		
		Table[
			filestocopy=FileNames["*.txt",inputdirs[[i]]];
			Table[
				CopyFile[filestocopy[[j]],FileNameJoin[{tempdir,FileNameTake[filestocopy[[j]]]}]];
				,{j,1,Length[filestocopy]}];
			,{i,1,Length[inputdirs]}
		];
				
		inputfiles=FileNames["*.txt"];
		
		Run["ssh "<>server<>" \"mkdir -p /linuxhome/tmp/jelmer/"<>runname<>"\""];
		Run["scp "tempdir<>"/*.txt "<>server<>":/linuxhome/tmp/jelmer/"<>runname<>"/"];
		Run["ssh "<>server<>" \"cp ~/Project/Main/CNetwork/main /linuxhome/tmp/jelmer/"<>runname<>"/\""];
		
		runs=Table[
			"./main "<>parameters[[j]]<>" --input "<>inputfiles[[i]]
			,{i,1,Length[inputfiles]}
			,{j,1,Length[parameters]}
		]//Flatten;
		
		runs=Partition[runs,IntegerPart[Length[runs]/nproc],IntegerPart[Length[runs]/nproc],1,{}];
		
		Table[
			Export[ToString[i]<>".sh",runs[[i]],"Text"]
		,{i,1,Length[runs]}];

		Export["run.sh",
			Table[
				"screen -d -m "<>ToString[i]<>".sh"
			,{i,1,Length[runs]}]
		,"Text"];
		
		Run["scp "tempdir<>"/*.sh "<>server<>":/linuxhome/tmp/jelmer/"<>runname<>"/"];
		
		Run["ssh "<>server<>" \"chmod +x /linuxhome/tmp/jelmer/"<>runname<>"/*.sh\""];
		Run["ssh "<>server<>" \"cd /linuxhome/tmp/jelmer/"<>runname<>"/; ./run.sh\""];
		
		DeleteDirectory[tempdir,DeleteContents->True];
		SetDirectory[initialdirectory];
		
		"As above, so below!"
	)]

PrepareRun[inputdirs_List,parameters_List,runname_String,nproc_Integer,outputdir_String]:=
	Module[{filestocopy,inputfiles,runs},(
		SetDirectory[outputdir];
		
		Table[
			filestocopy=FileNames["*.txt",inputdirs[[i]]];
			Table[
				CopyFile[filestocopy[[j]],FileNameJoin[{Directory[],FileNameTake[filestocopy[[j]]]}]];
				,{j,1,Length[filestocopy]}];
			,{i,1,Length[inputdirs]}
		];
		
		inputfiles=FileNames["*.txt"];
		
		runs=Table[
			"./main "<>parameters[[j]]<>" --input "<>inputfiles[[i]]
			,{i,1,Length[inputfiles]}
			,{j,1,Length[parameters]}
		]//Flatten;
		runs=Partition[runs,IntegerPart[Length[runs]/nproc],IntegerPart[Length[runs]/nproc],1,{}];
		
		Table[
			Export[ToString[i]<>".sh",runs[[i]],"Text"]
		,{i,1,Length[runs]}];

		Export["run.sh",
			Table[
				"screen -d -m "<>ToString[i]<>".sh"
			,{i,1,Length[runs]}]
		,"Text"];
		
)]

ResultsIndex[inputdir_String]:=
	Module[{filenames,datasets,methods,networks,variants,result,x},(
		filenames=FileNames["result*.txt",inputdir];
		datasets=Union[Flatten[StringCases[filenames,"~"~~x:RegularExpression["\\D+"]~~__~~".txt"->x]]];
		
		result = 
			Table[
				{
					i,
					datasets[[i]],
					methods=Union[Flatten[StringCases[filenames,x:RegularExpression["[01]{3,3}\\d+"]~~"~"~~datasets[[i]]~~__~~".txt"->x]]],
					networks=Table[Length[Union[Flatten[StringCases[filenames,methods[[j]]~~"~"~~datasets[[i]]~~x:RegularExpression["\\d+"]~~__~~".txt"->x]]]],{j,1,Length[methods]}],
					variants=Table[Length[Flatten[StringCases[filenames,methods[[j]]~~"~"~~datasets[[i]]~~ToString[First[networks]]~~"-"~~x:RegularExpression["\\d+"]~~".txt"->x]]],{j,1,Length[methods]}]
				}
			,{i,1,Length[datasets]}];
		
		PrependTo[result,{"#","Dataset","Method","# Networks","# Variants"}];
		Grid[result,Frame->All]
	)]

End[]


EndPackage[]
