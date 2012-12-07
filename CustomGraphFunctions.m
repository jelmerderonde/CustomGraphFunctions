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
ResultsIndex::usage = "ResultsIndex[inputdir] searches the directory inputdir for result files and displays the available run results as a table.";
PrepareRun::usage = "PrepareRun[inputdirs,parameters,runname,nproc,outputdir] prepares a run in the outputdir with the files in inputdirs as input.";
DomainSizesHistogram::usage = "DomainSizesHistogram[data] returns a log log histogram of domain sizes.";

readResult::usage = "readResult[name,inputfile] reads and interprets the inputfile and assigns UpValues (pseudofunctions) to name which allow results to be easily read out.";

networkName::usage = "networkName[result] returns the name of the network.";
networkID::usage = "networkID[result] returns the network ID.";
variantID::usage = "variantID[result] returns the variant ID.";
initialStatesCount::usage = "initialStatesCount[result] returns the number of initial states used in this run.";
vertexCount::usage = "vertexCount[result] returns the number of vertices in het network.";
edgeCount::usage = "edgeCount[result] returns the number of edges in the network.";
convergingStatesRatio::usage = "convergingStatesRatio[result] returns the ratio of states that converged to an attractor.";
synchronousQ::usage = "synchronousQ[result] returns a boolean whether the run was synchronously updated.";
randomOrderQ::usage = "randomOrderQ[result] returns a boolean whether the update order of the run was random.";
decayCounter::usage = "decayCounter[result] returns the decay counter.";
falseFeedbackQ::usage = "falseFeedbackQ[result] returns a boolean whether the false feedback was on in the run.";
attractors::usage = "attractors[result] returns all (non-zero) found attractors.";
attractorCount::usage = "attractorCount[result] returns the number of (non-zero) attractors found.";
attractorLengths::usage = "attractorLengths[result] returns all the cycle lengths of the (non-zero) found attractors.";
domainSizes::usage = "domainSizes[result] returns all the domain sizes of the (non-zero) found attractors.";
transientLengths::usage = "transientLengths[result] returns all the transients of the (non-zero) found attractors.";
activeNodeCount::usage = "activeNodeCount[result] the number of nodes that are active in the (non-zero) found attractors.";

readResultDirectory::usage = "readResultDirectory[inputdir] reads all the result files in inputdir, creates symbols for them and returns a list with the symbols.";
sortResultSymbols::usage = "sortResultSymbols[symbols] works with the output (or a subset of the output) of readResultDirectory and sorts the symbols into lists of the same network.";
resultRow::usage = "resultRow[symbols] uses a set of result symbols to create a formatted output row of analyzed data";
resultTable::usage = "resultTable[symbols] creates resultRows of all sets of symbols and displays it in a single grid.";
fullResultTable::usage = "fullResultTable[inputdir] gives the resultTable of all the files in inputdir. Use with caution!";


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

HierarchyHistogram[graph_Graph,nlevels_Integer,server_String,opts:OptionsPattern[]]:=
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
			opts,
			ChartStyle->colors,
			ChartLayout->"Stacked",
			ChartLegends->ToString/@Range[Max[levels]]
		]
	)]

LevelInteractions[graph_Graph,nlevels_Integer,server_String,opts:OptionsPattern[]]:=
	Module[{s,levels},(
		levels=HierarchyLevels[graph,nlevels,server];
		s=SparseArray[
			Apply[Rule,
				Apply[List,
					DeleteCases[Tally[EdgeList[graph]/.Table[Rule[VertexList[graph][[v]],levels[[v]]],{v,1,VertexCount[graph]}]],
			{0\[DirectedEdge]0,_}],2],1]];
		ArrayPlot[
			s,
			opts,
			Mesh->True,
			Epilog->{Red,MapIndexed[Text[#1,Reverse[#2-1/2]]&,Reverse[s],{2}]}
		]
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
					variants=Table[Length[Flatten[StringCases[filenames,methods[[j]]~~"~"~~datasets[[i]]~~ToString[First[networks]]~~"-"~~x:RegularExpression["\\d+"]~~".txt"->x]]],{j,1,Length[methods]}],
					Total[networks*variants]
				}
			,{i,1,Length[datasets]}];
		
		PrependTo[result,{"#","Dataset","Method","# Networks","# Variants","# Files"}];
		Grid[result,Frame->All]
	)]

readResult[name_Symbol,inputfile_String]:=
	Module[{rawdata,networkname,networkid,variantid},(
		Unprotect[name];
		ClearAll[name];
		rawdata=ReadList[inputfile];
		rawdata=DeleteCases[rawdata,{0,0,{0},0},\[Infinity]];
		
		networkName[name]^=StringCases[inputfile,__~~"~"~~networkname:RegularExpression["\\D+"]->networkname][[1]];
		networkID[name]^=StringCases[inputfile,__~~"~"~~RegularExpression["\\D+"]~~networkid:RegularExpression["\\d+"]->networkid][[1]];
		variantID[name]^=StringCases[inputfile,__~~"~"~~RegularExpression["\\D+"]~~RegularExpression["\\d+"]~~"-"~~variantid:RegularExpression["\\d+"]->variantid][[1]];
		initialStatesCount[name]^=rawdata[[2,2]];
		vertexCount[name]^=rawdata[[2,4]];
		edgeCount[name]^=rawdata[[2,5]];
		convergingStatesRatio[name]^=Total[rawdata[[4;;-1,2]]]/rawdata[[2,2]];
		synchronousQ[name]^=If[rawdata[[2,6]]==1,True,False];
		randomOrderQ[name]^=If[rawdata[[2,7]]==1,True,False];
		decayCounter[name]^=rawdata[[2,8]];
		falseFeedbackQ[name]^=If[rawdata[[2,9]]==1,True,False];
		attractors[name]^=rawdata[[4;;-1]];
		attractorCount[name]^=Length[rawdata]-4;
		attractorLengths[name]^=rawdata[[4;;-1,1]];
		domainSizes[name]^=rawdata[[4;;-1,2]];
		transientLengths[name]^=rawdata[[4;;-1,3]];
		activeNodeCount[name]^=rawdata[[4;;-1,4]];

		Protect[name];
		name
	)]

readResultDirectory[inputdir_String]:=
	Module[{initialdirectory,datasets,files,symbolscreated,basename},(
		initialdirectory=Directory[];
		SetDirectory[inputdir];
		datasets=ResultsIndex[Directory[]][[1,2;;-1,2]];
		symbolscreated={};
		
		Table[
			basename=datasets[[i]];
			files=FileNames["*"~~basename~~"*.txt"];
			
			Table[
				readResult[ToExpression[basename~~ToString[j]],files[[j]]];
				AppendTo[symbolscreated,
					{
						ToExpression[basename~~ToString[j]],
						networkName[ToExpression[basename~~ToString[j]]],
						networkID[ToExpression[basename~~ToString[j]]],
						variantID[ToExpression[basename~~ToString[j]]],
						synchronousQ[ToExpression[basename~~ToString[j]]],
						randomOrderQ[ToExpression[basename~~ToString[j]]],
						decayCounter[ToExpression[basename~~ToString[j]]],
						falseFeedbackQ[ToExpression[basename~~ToString[j]]]
					}];
			,{j,1,Length[files]}]
		,{i,1,Length[datasets]}];
		
		SetDirectory[initialdirectory];
		symbolscreated
	)]

DomainSizesHistogram[data_List,opts:OptionsPattern[]]:=
	Module[{},(
		Histogram[
			Log[10,data],
			{-0.05,5.05,0.1},
			"LogCount",
			opts,
			AxesLabel->{"Basin of attraction", "Number of attractors"},
			PlotRange->{Automatic,{-1,9}},
			Ticks->{CustomTicks`LogTicks[0,5],Automatic}
		]
)]
sortResultSymbols[symbols_List]:=
	Module[{patterns},(
		patterns=symbols[[All,2;;4]]//Union;
		Table[
			Cases[symbols,{_,patterns[[i,1]],patterns[[i,2]],patterns[[i,3]],_,_,_,_}]
		,{i,1,Length[patterns]}]
	)]

resultRow[symbols_List]:=
	Module[{methods,output,methodoutput,methodwidth,header1,header2,methodheader1,methodheader2},(
		(*Generating a list of methods*)
		methods=symbols[[All,5;;8]]//Union//Sort;
		
		(*Starting the output row*)
		output=
			{
				symbols[[1,2]],
				symbols[[1,3]],
				symbols[[1,4]],
				Column[{
					"# States: "~~ToString[initialStatesCount[symbols[[1,1]]]],
					"# Vertices: "~~ToString[vertexCount[symbols[[1,1]]]],
					"# Edges: "~~ToString[edgeCount[symbols[[1,1]]]]
				}]
			};
		
		(*Generating specific output per method*)
		methodoutput={};
		Table[
			AppendTo[methodoutput,
				{
					Column[{
						"Data: "~~ToString[symbols[[i,1]]],
						"# Attractors: "~~ToString[attractorCount[symbols[[i,1]]]],
						"% Converging: "~~ToString[convergingStatesRatio[symbols[[i,1]]]*100//N]~~"%"
					}],
					domainSizesHistogram[
						domainSizes[symbols[[i,1]]],
						ImageSize->150,
						AxesLabel->None,
						Ticks->None
					]
				}
			],
		{i,1,Length[methods]}];
		methodwidth=Length[methodoutput[[1]]];(*Saving the number of columns per method*)
		methodoutput=Flatten[methodoutput];
		
		(*Adding the method output to the output*)
		Table[
			AppendTo[output,
				methodoutput[[i]]
			],
		{i,1,Length[methodoutput]}];
		
		(*Building the header containg info about the different methods*)
		header1={"Network",SpanFromLeft,SpanFromLeft,SpanFromLeft};

		methodheader1=Table[
			{
				Column[{
					"Updating: "~~If[synchronousQ[symbols[[i,1]]],"Synchronous","Asynchronous"],
					"Decay counter: "~~ToString[decayCounter[symbols[[i,1]]]],
					"False feedback: "~~If[falseFeedbackQ[symbols[[i,1]]],"Yes","No"]
				}],
				Table[
					SpanFromLeft,
				{methodwidth-1}]//Flatten
			},
		{i,1,Length[methods]}];

		methodheader1=Flatten[methodheader1];
		
		Table[
			AppendTo[header1,
				methodheader1[[i]]
			],
		{i,1,Length[methodheader1]}];
		
		(*Building the header containg the column titles*)
		header2={"Name","ID","Var","Info"};
		methodheader2=Table[{"Runinfo","Domain sizes"},{Length[methods]}];
		methodheader2=Flatten[methodheader2];
	
		Table[
			AppendTo[header2,
				methodheader2[[i]]
			],
		{i,1,Length[methodheader2]}];

		(*Generating the output grid*)
		Grid[{header1,header2,output},Frame->All]
	)]

resultTable[symbols_List]:=
	Module[{output},(
		output=resultRow[symbols[[1]]][[1,1;;2]];
		Table[
			AppendTo[output,resultRow[symbols[[i]]][[1,3]]];
		,{i,1,Length[symbols]}];
		Grid[output,Frame->All]
	)]

fullResultTable[inputdir_String]:=
	resultTable[sortResultSymbols[readResultDirectory[inputdir]]]

End[]


EndPackage[]
