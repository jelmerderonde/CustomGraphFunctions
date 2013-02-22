(* ::Package:: *)

BeginPackage["CustomGraphFunctions`"];


generateTopology::usage = "generateTopology[g] generates a list whose elements encode edges in directed graph g with weights";
topologyList::usage = "topologyList[g] generates output of directed graph g suitable for export to use with CNetwork.";
generateGraph::usage = "generateGraph[topology] returns a graph based on the topology part of a CNetwork result file.";
encodeGraph::usage = "encodeGraph[seed,weights] generates a directed graph from the ENCODE consortium and uses seed to generate random weights, picked from list w.";
getSelfLoops::usage = "getSelfLoops[graph] gives a list of {node number, weight} of graph.";
removeSL::usage = "removeSL[graph] removes self-loops from a graph and returns a new graph.";
addSL::usage = "addSL[graph,n,seed] adds n new self loops to the graph using seed for randomization and returns the graph.";
getWeightMap::usage = "getWeightMap[graph] returns the weightmap of a graph.";
randomIOGraph::usage = "randomIOGraph[g,i,interval,seed,keepselfloops] generates a random graph with the same In/Out degree distribution as graph g, by shuffeling random edges i times. Interval specifies how often intermediate results should be returned. Seed sets the random seed. keepselfsloops is a boolean. If set to True, the algorithm will not shuffle self-loops.";
randomVDGraph::usage = "randomVDGraph[g,i,interval,seed,keepselfloops] generates a random graph with the same total degree distribution as graph g, by shuffeling random edges i times. Interval specifies how often intermediate results should be returned. Seed sets the random seed. keepselfsloops is a boolean. If set to True, the algorithm will not shuffle self-loops.";
randomAllGraph::usage = "randomAllGraph[g,i,interval,seed,keepselfloops] generates a random graph from graph g, by shuffeling random edges i times. Interval specifies how often intermediate results should be returned. Seed sets the random seed. keepselfsloops is a boolean. If set to True, the algorithm will not shuffle self-loops.";
hDegree::usage = "hDegree[g,v] returns the hierarchy degree of vertex v of graph g.";
countSelfLoops::usage = "countSelfLoops[g] returns the number of self loops in graph g.";
hierarchyLevels::usage = "hierarchyLevels[g,n,s] gives a list of n hierarchy levels of graph g, by sshing to server s and executing a Matlab script.";
hierarchyHistogram::usage = "hierarchyHistogram[g,n,s] gives a histogram of n hierarchy levels of graph g, by sshing to server s and executing a Matlab script.";
levelInteractions::usage = "levelInteractions[g,n,s] gives a level interaction matrix of n hierarchy levels of graph g, by sshing to server s and executing a Matlab script.";
resultsIndex::usage = "resultsIndex[inputdir] searches the directory inputdir for result files and displays the available run results as a table.";
prepareRun::usage = "prepareRun[inputdirs,parameters,nproc,outputdir] prepares a run in the outputdir with the files in inputdirs as input.";
domainSizesHistogram::usage = "domainSizesHistogram[data] returns a log log histogram of domain sizes.";
getAttractorProfile::usage = "getAttractorProfile[resultsymbol] returns the attractorProfile of a result symbol.";

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
fullResults::usage = "fullResults[result] returns all (non-zero) found attractors, including statistics.";
attractors::usage = "attractors[result] returns all (non-zero) found attractor states.";
attractorCount::usage = "attractorCount[result] returns the number of (non-zero) attractors found.";
attractorLengths::usage = "attractorLengths[result] returns all the cycle lengths of the (non-zero) found attractors.";
domainSizes::usage = "domainSizes[result] returns all the domain sizes of the (non-zero) found attractors.";
transientLengths::usage = "transientLengths[result] returns all the transients of the (non-zero) found attractors.";
activeNodeCount::usage = "activeNodeCount[result] the number of nodes that are active in the (non-zero) found attractors.";
graph::usage = "graph[result] returns the graph of the network used in the run.";
resultFile::usage = "resultFile[result] returns a string with the location of the inputfile used to create the result.";

readResultDirectory::usage = "readResultDirectory[inputdir] reads all the result files in inputdir, creates symbols for them and returns a list with the symbols.";
sortResultSymbols::usage = "sortResultSymbols[symbols] works with the output (or a subset of the output) of readResultDirectory and sorts the symbols into lists of the same network.";
resultRow::usage = "resultRow[symbols,server] uses a set of result symbols to create a formatted output row of analyzed data";
resultTable::usage = "resultTable[symbols,server] creates resultRows of all sets of symbols and displays it in a single grid.";
fullResultTable::usage = "fullResultTable[inputdir,server] gives the resultTable of all the files in inputdir. Use with caution!";


Begin["`Private`"]

generateTopology[graph_Graph]:=
	Module[{edges,translationRules,data},(
		translationRules=Table[Rule[VertexList[graph][[i]],i],{i,1,VertexCount[graph]}];
		edges=Map[#->PropertyValue[{graph,#},EdgeWeight]&,EdgeList[graph]];
		data=Replace[edges/.translationRules,{DirectedEdge->List,Rule->List},\[Infinity],Heads->True];
		Table[Insert[data[[i,1]],data[[i,2]],2],{i,1,EdgeCount[graph]}]
	)]

topologyList[graph_Graph]:=
	Module[{output},(
		output=generateTopology[graph];
		PrependTo[output,VertexCount[graph]]
	)]

generateGraph[topology_List]:=
	Module[{vertices,edges,weights},(
		vertices=Range[topology[[2,1]]];
		edges=Replace[#,List->DirectedEdge,Infinity,Heads->True]&/@topology[[4;;-1,{1,3}]];
		weights=topology[[4;;-1,2]];
		
		Graph[vertices,edges,EdgeWeight->weights]
	)]

encodeGraph[seed_Integer,w_List] :=
	Module[{encodeData,tfs,encodeEdges,weights},(
		SeedRandom[seed];
		weights=Table[RandomChoice[w],{323}];
		encodeData=Import["/Users/jelmerderonde/Documents/Code/CNetwork/NETWORKS/ENCODE/enets2.Proximal_filtered.txt",{"Text","Words"}];
		tfs=Import["/Users/jelmerderonde/Documents/Code/CNetwork/NETWORKS/ENCODE/tfs.txt",{"Text","Words"}];
		encodeEdges=Cases[Partition[encodeData,2],{Apply[Alternatives,tfs],Apply[Alternatives,tfs]}];
		Graph[Apply[DirectedEdge,encodeEdges,2],EdgeWeight->weights]
	)]

getSelfLoops[graph_Graph]:=
	Module[{weightMap,x,w},(
		weightMap=getWeightMap[graph];
		Cases[weightMap,HoldPattern[x_\[DirectedEdge]x_->w_]->{x,w}]
	)]

removeSL[graph_]:=
	Module[{vertices,weightMap,newMap},(
		vertices=VertexList[graph];
		weightMap=Map[#->PropertyValue[{graph,#},EdgeWeight]&,EdgeList[graph]];
		newMap=DeleteCases[weightMap,Rule[x_\[DirectedEdge]x_,_]];
		
		Graph[vertices,newMap[[All,1]],EdgeWeight->newMap[[All,1]]/.newMap]
	)]

addSL[graph_Graph,number_Integer,seed_Integer]:=
	Module[{weightMap,vertices,i,weights,testVertex,edges},(
		SeedRandom[seed];
		weightMap=getWeightMap[graph];
		vertices=VertexList[graph];
		edges=EdgeList[graph];
		weights=Union[weightMap[[All,2]]];
		i=0;
		
		While[i<number,
			testVertex=vertices[[RandomInteger[{1,VertexCount[graph]}]]];
			If[Length[Cases[edges,testVertex\[DirectedEdge]testVertex]]==0,
				(*New selfloop doesn't exist yet, lets add it.*)
				AppendTo[edges,testVertex\[DirectedEdge]testVertex];
				AppendTo[weightMap,testVertex\[DirectedEdge]testVertex->RandomChoice[weights]];
				i++;
			];
		];
		Graph[vertices,edges,EdgeWeight->edges/.weightMap]
	)]

getWeightMap[graph_Graph]:=Map[#->PropertyValue[{graph,#},EdgeWeight]&,EdgeList[graph]];

randomIOGraph[graph_Graph,max_Integer,interval_Integer,seed_Integer,keepSelfLoops_Symbol]:=
	Module[{newGraph,weightMap,testEdges,newMaps,out,i,result},(
		result={};
		newGraph=graph;
		weightMap=getWeightMap[graph];
		i=0;
		SeedRandom[seed];

		While[i<=max,
			testEdges=Table[RandomChoice[EdgeList[newGraph]],{2}];
			If[
				Length[EdgeList[newGraph,testEdges[[2,2]]\[DirectedEdge]testEdges[[1,2]]]]==Length[EdgeList[newGraph,testEdges[[2,1]]\[DirectedEdge]testEdges[[1,1]]]]==0 && Length[Union[testEdges]]==2 &&
					If[keepSelfLoops,If[testEdges[[1,1]]!=testEdges[[1,2]] && testEdges[[2,1]]!=testEdges[[2,2]],True,False],True],

				newMaps={testEdges[[2,2]]\[DirectedEdge]testEdges[[1,2]]->testEdges[[1]]/.weightMap,testEdges[[2,1]]\[DirectedEdge]testEdges[[1,1]]->testEdges[[2]]/.weightMap};

				weightMap=DeleteCases[weightMap,testEdges[[1]]->_];
				weightMap=DeleteCases[weightMap,testEdges[[2]]->_];
				weightMap=Join[weightMap,newMaps];

				newGraph=EdgeDelete[newGraph,testEdges];
				newGraph=EdgeAdd[newGraph,{testEdges[[2,2]]\[DirectedEdge]testEdges[[1,2]],testEdges[[2,1]]\[DirectedEdge]testEdges[[1,1]]}];
				i++;
				If[Mod[i,interval]==0,
					AppendTo[result,Graph[VertexList[newGraph],EdgeList[newGraph],EdgeWeight->EdgeList[newGraph]/.weightMap]];
				];
			];
		];
		result
	)]

randomVDGraph[graph_Graph,max_Integer,interval_Integer,seed_Integer,keepSelfLoops_Symbol]:=
	Module[{newGraph,weightMap,testEdges,newMaps,out,i,x,result,dSLCounter},(
		result={};
		newGraph=graph;
		weightMap=getWeightMap[graph];
		i=0;
		dSLCounter=0;
		SeedRandom[seed];

		While[i<=max,
			testEdges=Table[RandomChoice[EdgeList[newGraph]],{2}];
			If[
				Length[EdgeList[newGraph,testEdges[[2,2]]\[DirectedEdge]testEdges[[1,2]]]]==Length[EdgeList[newGraph,testEdges[[2,1]]\[DirectedEdge]testEdges[[1,1]]]]==0 && Length[Union[testEdges]]==2 &&
					If[keepSelfLoops,If[testEdges[[1,1]]!=testEdges[[1,2]] && testEdges[[2,1]]!=testEdges[[2,2]],True,False],True] &&
					If[Length[Cases[testEdges,x_\[DirectedEdge]x_]]==2,dSLCounter++;False,True],

				newMaps={testEdges[[2,2]]\[DirectedEdge]testEdges[[1,2]]->testEdges[[1]]/.weightMap,testEdges[[2,1]]\[DirectedEdge]testEdges[[1,1]]->testEdges[[2]]/.weightMap};

				weightMap=DeleteCases[weightMap,testEdges[[1]]->_];
				weightMap=DeleteCases[weightMap,testEdges[[2]]->_];
				weightMap=Join[weightMap,newMaps];

				newGraph=EdgeDelete[newGraph,testEdges];
				newGraph=EdgeAdd[newGraph,{testEdges[[2,2]]\[DirectedEdge]testEdges[[1,2]],testEdges[[2,1]]\[DirectedEdge]testEdges[[1,1]]}];
				i++;
				If[Mod[i,interval]==0,
					AppendTo[result,Graph[VertexList[newGraph],EdgeList[newGraph],EdgeWeight->EdgeList[newGraph]/.weightMap]];
				];
			];
		];
		Print[dSLCounter];
		result
	)]

randomAllGraph[graph_Graph,max_Integer,interval_Integer,seed_Integer,keepSelfLoops_Symbol]:=
	Module[{newGraph,weightMap,testEdges,newEdges,newMaps,out,i,result},(
		result={};
		newGraph=graph;
		weightMap=getWeightMap[graph];
		i=0;
		SeedRandom[seed];

		While[i<=max,
			testEdges=Table[RandomChoice[EdgeList[newGraph]],{2}];
			newEdges=Table[RandomChoice[VertexList[newGraph]]\[DirectedEdge]RandomChoice[VertexList[newGraph]],{2}];
			If[
				Length[Union[testEdges]] == Length[Union[newEdges]] == 2 && Length[EdgeList[newGraph,newEdges[[1]]]] == Length[EdgeList[newGraph,newEdges[[2]]]] == 0 &&
					If[keepSelfLoops,If[testEdges[[1,1]]!=testEdges[[1,2]] && testEdges[[2,1]]!=testEdges[[2,2]],True,False],True],

				newMaps={newEdges[[1]]->testEdges[[1]]/.weightMap,newEdges[[2]]->testEdges[[2]]/.weightMap};

				weightMap=DeleteCases[weightMap,testEdges[[1]]->_];
				weightMap=DeleteCases[weightMap,testEdges[[2]]->_];
				weightMap=Join[weightMap,newMaps];

				newGraph=EdgeDelete[newGraph,testEdges];
				newGraph=EdgeAdd[newGraph,newEdges];
				i++;
				If[Mod[i,interval]==0,
					AppendTo[result,Graph[VertexList[newGraph],EdgeList[newGraph],EdgeWeight->EdgeList[newGraph]/.weightMap]];
				];
			];
		];
		result
	)]

hDegree[graph_Graph,v_]:=
	(VertexOutDegree[graph,v]-VertexInDegree[graph,v])/(VertexOutDegree[graph,v]+VertexInDegree[graph,v])

hierarchyLevels[graph_Graph,nlevels_Integer,server_String]:=
	Module[{initialDirectory,serverFolder,levels,tempDir},(
		tempDir=CreateDirectory[];
		initialDirectory=Directory[];
		serverFolder=server<>":Project/Code/Matlab/Automated/";
		
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

hierarchyHistogram[graph_Graph,nlevels_Integer,server_String,opts:OptionsPattern[]]:=
	Module[{colors,levels,data,h,v,l},(
		colors={Green,Blue,Red,Purple,Orange,Cyan};
		levels=hierarchyLevels[graph,nlevels,server];
		data=Table[
			Cases[
				Table[
					If[VertexDegree[graph][[v]]>0,
						{hDegree[graph,VertexList[graph][[v]]],levels[[v]]}
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
			PlotRange->{Automatic,{0,30}},
			ChartStyle->colors,
			ChartLayout->"Stacked",
			ChartLegends->ToString/@Range[Max[levels]]
		]
	)]

countSelfLoops[g_Graph]:=
	Module[{x},
		Cases[EdgeList[g],x_\[DirectedEdge]x_]//Length
	]

levelInteractions[graph_Graph,nlevels_Integer,server_String,opts:OptionsPattern[]]:=
	Module[{s,levels},(
		levels=hierarchyLevels[graph,nlevels,server];
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

domainSizesHistogram[data_List,opts:OptionsPattern[]]:=
	Module[{},(
		Histogram[
			Log[10,data],
			{-0.05,5.05,0.1},
			"LogCount",
			opts,
			AxesLabel->{"Basin of attraction", "Number of attractors"},
			AxesOrigin->{-0.05,-1},
			PlotRange->{Automatic,{-1,9}},
			Ticks->{CustomTicks`LogTicks[0,5],Automatic}
		]
)]

getAttractorProfile[resultSymbol_Symbol]:=
	Module[{data,domains,lengths,result},(
		data=ToExpression/@Characters[attractors[resultSymbol]];
		domains=domainSizes[resultSymbol];
		lengths=attractorLengths[resultSymbol];
		
		result={};
		Table[
			AppendTo[result,domains[[i]]*data[[i]]/lengths[[i]]];
			,{i,1,Length[data]}
		];

		Total[Flatten[result,1]]/Table[convergingStatesRatio[resultSymbol]*initialStatesCount[resultSymbol],{vertexCount[resultSymbol]}]
	)]

prepareRun[inputdirs_List,parameters_List,nproc_Integer,outputdir_String]:=
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

resultsIndex[inputdir_String]:=
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
	Module[{rawdata,networkname,networkid,variantid,topology,nedges,attrout},(
		Unprotect[name];
		ClearAll[name];
		rawdata=ReadList[inputfile];
		

		nedges=rawdata[[2,2]];
		topology=rawdata[[1;;3+nedges]];
		rawdata=Delete[rawdata,Partition[Range[1,3+nedges],1]];
		
		If[Length[rawdata[[3]]]>4,attrout=1,attrout=0];
		rawdata=DeleteCases[rawdata,If[attrout==1,{0,{},0,{0},0},{0,0,{0},0}],\[Infinity]];

		networkName[name]^=StringCases[inputfile,__~~"~"~~networkname:RegularExpression["\\D+"]->networkname][[1]];
		networkID[name]^=StringCases[inputfile,__~~"~"~~RegularExpression["\\D+"]~~networkid:RegularExpression["\\d+"]->networkid][[1]];
		variantID[name]^=StringCases[inputfile,__~~"~"~~RegularExpression["\\D+"]~~RegularExpression["\\d+"]~~"-"~~variantid:RegularExpression["\\d+"]->variantid][[1]];
		initialStatesCount[name]^=rawdata[[2,2]];
		vertexCount[name]^=rawdata[[2,4]];
		edgeCount[name]^=rawdata[[2,5]];
		convergingStatesRatio[name]^=Total[rawdata[[4;;-1,2+attrout]]]/rawdata[[2,2]];
		synchronousQ[name]^=If[rawdata[[2,6]]==1,True,False];
		randomOrderQ[name]^=If[rawdata[[2,7]]==1,True,False];
		decayCounter[name]^=rawdata[[2,8]];
		falseFeedbackQ[name]^=If[rawdata[[2,9]]==1,True,False];
		fullResults[name]^=rawdata[[4;;-1]];
		If[attrout==1,attractors[name]^=rawdata[[4;;-1,2]]];
		attractorCount[name]^=Length[rawdata]-3;
		attractorLengths[name]^=rawdata[[4;;-1,1]];
		domainSizes[name]^=rawdata[[4;;-1,2+attrout]];
		transientLengths[name]^=rawdata[[4;;-1,3+attrout]];
		activeNodeCount[name]^=rawdata[[4;;-1,4+attrout]];
		graph[name]^=generateGraph[topology];
		resultFile[name]^=inputfile;

		Protect[name];
		name
	)]

readResultDirectory[inputdir_String]:=
	Module[{initialdirectory,datasets,files,symbolscreated,basename},(
		initialdirectory=Directory[];
		SetDirectory[inputdir];
		datasets=resultsIndex[Directory[]][[1,2;;-1,2]];
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

sortResultSymbols[symbols_List]:=
	Module[{patterns},(
		patterns=symbols[[All,2;;4]]//Union;
		Table[
			Cases[symbols,{_,patterns[[i,1]],patterns[[i,2]],patterns[[i,3]],_,_,_,_}]
		,{i,1,Length[patterns]}]
	)]

resultRow[symbols_List,server_String]:=
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
		
		(*Adding more general info about the network*)
		AppendTo[output,hierarchyHistogram[graph[symbols[[1,1]]],3,server,ImageSize->150,Ticks->None,ChartLegends->None]];
		
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
		
		AppendTo[header1,"Network hierarchy"];
		
		(*Building the header containg the column titles*)
		header2={"Name","ID","Var","Info"};
		methodheader2=Table[{"Runinfo","Domain sizes"},{Length[methods]}];
		methodheader2=Flatten[methodheader2];
	
		Table[
			AppendTo[header2,
				methodheader2[[i]]
			],
		{i,1,Length[methodheader2]}];
		
		AppendTo[header2,"Hierarchy"];


		(*Generating the output grid*)
		Grid[{header1,header2,output},Frame->All]
	)]

resultTable[symbols_List,server_String]:=
	(
	j=0;
	Monitor[
		Module[{output},(
			output=resultRow[symbols[[1]],server][[1,1;;2]];
			Table[
				AppendTo[output,resultRow[symbols[[i]],server][[1,3]]];
				j=i;
			,{i,1,Length[symbols]}];
			Grid[output,Frame->All]
		)],
	
		Column[{Row[{j,"/",Length[symbols]}," "],Row[{ProgressIndicator[j,{0,Length[symbols]}],j/Length[symbols]*100//N,"%"}," "]}]
	]
	)

fullResultTable[inputdir_String,server_String]:=
	resultTable[sortResultSymbols[readResultDirectory[inputdir]],server]

End[]


EndPackage[]
