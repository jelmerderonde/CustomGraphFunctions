(* ::Package:: *)

BeginPackage["CustomGraphFunctions`"];


generateTopology::usage = "generateTopology[g] generates a list whose elements encode edges in directed graph g with weights";
topologyList::usage = "topologyList[g] generates output of directed graph g suitable for export to use with CNetwork.";
generateGraph::usage = "generateGraph[topology] returns a graph based on the topology part of a CNetwork result file.";
regenerateGraph::usage = "regenerateGraph[topology]returns a graph based on an imported topology file. (Import[\"file.txt\",\"Data\"]).";
encodeGraph::usage = "advancedEncodeGraph[seed,type] generates a directed graph from the ENCODE consortium and uses seed to generate unknown weights. Type is a string that determines which network to return. Options are: full, proximal and distal.";
getSelfLoops::usage = "getSelfLoops[graph] gives a list of {node number, weight} of graph.";
countLooseNodes::usage = "countLooseNodes[graph] counts the number of nodes with no connecting edges or only a selfloop.";
getLooseNodes::usage = "getLooseNodes[graph] returns a list of nodes with no connecting edges or only a selfloop.";
weightDistGraph::usage = "weightDistGraph[graph,{actRatio,reprRatio,randRatio},seed] assigns new weights to edges. The second argument determines the ratio of activating, repressing and mixed vertices..";
removeSL::usage = "removeSL[graph, seed] removes self-loops from a graph by intelligent reshuffling using seed for randomization and returns a new graph.";
bruteRemoveSL::usage = "bruteRemoveSL[graph] removes all self-loops from a graph. Nothing intelligent about it.";
addSL::usage = "addSL[graph,n,seed] adds n new self loops to the graph by intelligent reshuffling using seed for randomization and returns the graph.";
addSpecificSL::usage = "addSpecificSL[graph, vertex, weight], adds a selfloop to vertex in graph with weight. Only if it doesn' exist yet.";
addRandomEdges::usage = "addRandomEdges[graph,n,seed] adds n new random edges to the graph and returns the new graph.";
getWeightMap::usage = "getWeightMap[graph] returns the weightmap of a graph.";
randomIOGraph::usage = "randomIOGraph[g,i,interval,seed,keepselfloops] generates a random graph with the same In/Out degree distribution as graph g, by shuffeling random edges i times. Interval specifies how often intermediate results should be returned. Seed sets the random seed. keepselfsloops is a boolean. If set to True, the algorithm will not shuffle self-loops.";
randomVDGraph::usage = "randomVDGraph[g,i,interval,seed,keepselfloops] generates a random graph with the same total degree distribution as graph g, by shuffeling random edges i times. Interval specifies how often intermediate results should be returned. Seed sets the random seed. keepselfsloops is a boolean. If set to True, the algorithm will not shuffle self-loops.";
randomAllGraph::usage = "randomAllGraph[g,i,interval,seed,keepselfloops] generates a random graph from graph g, by shuffeling random edges i times. Interval specifies how often intermediate results should be returned. Seed sets the random seed. keepselfsloops is a boolean. If set to True, the algorithm will not shuffle self-loops.";
hDegree::usage = "hDegree[g,v] returns the hierarchy degree of vertex v of graph g.";
countSelfLoops::usage = "countSelfLoops[g] returns the number of self loops in graph g.";
degreeHistogram::usage = "degreeHistogram[graph, type, options] gives a histogram of the vertexdegree's of graph. Type can be \"In\", \"Out\" or \"All\".";
resultsIndex::usage = "resultsIndex[inputdir] searches the directory inputdir for result files and displays the available run results as a table.";
prepareRun::usage = "prepareRun[inputdirs,parameters,nproc,outputdir] prepares a run in the outputdir with the files in inputdirs as input.";
advancedPrepareRun::usage = "advancedPrepareRun[inputDirs,parameters,nProc,initialStatesDir,outputDir] prepares a run in the outputdir with the files in inputdirs as input.";
domainSizesHistogram::usage = "domainSizesHistogram[data] returns a log log histogram of domain sizes.";
getAttractorProfile::usage = "getAttractorProfile[resultsymbol] returns the attractorProfile of a result symbol.";
attractorHistogram::usage = "attractorHistogram[symbols,opts] returns a histogram with the ratio of active nodes on horizontal axis and the frequency of attractors on vertical. Only accepts a list of 1 or more result symbols.";
createAttractorGraph::usage ="createAttractorGraph[attractor] creates an attractor graph from attractor strings saved in a result.";


Begin["`Private`"]

generateTopology[graph_Graph]:=
	Module[{weightMap,translationRules},(
		weightMap=getWeightMap[graph];
		translationRules=Rule@@@Partition[Riffle[Sort[VertexList[graph]],Range[VertexCount[graph]]],2];
		
		Table[{weight[[1,1]]/.translationRules,weight[[2]],weight[[1,2]]/.translationRules},{weight,weightMap}]
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

regenerateGraph[topology_List]:=
	Module[{vertices,edges,weights},(
		vertices=Range[topology[[1,1]]];
		edges=Replace[#,List->DirectedEdge,Infinity,Heads->True]&/@topology[[2;;-1,{1,3}]];
		weights=topology[[2;;-1,2]];
		
		Graph[vertices,edges,EdgeWeight->weights]
	)]

encodeGraph[seed_Integer,output_String:"proximal",opts:OptionsPattern[]]:=
	Module[{prefix,proximalData,distalData,tfData,proximalEdges,distalEdges,vertices,proximalWeights,distalWeights,vertex,edge,type,newEdges,allEdges,allWeights,testWeights,positions},(
		SeedRandom[seed];
		prefix="/path/to/encode/files/"
		
		(*Import data*)
		proximalData=DeleteCases[Import[prefix<>"enets2.Proximal_filtered.txt","Table"],{}];
		distalData=Import[prefix<>"enets3.Distal.txt","Table"][[All,{1,3}]];
		tfData=DeleteCases[Import[prefix<>"tfstatus.txt","Table"],{"TCF7L2",_}];
		
		(*Create edges*)
		proximalEdges=DeleteDuplicates[DirectedEdge@@@Cases[proximalData,{Alternatives@@tfData[[All,1]],Alternatives@@tfData[[All,1]]}]];
		distalEdges=DeleteDuplicates[DirectedEdge@@@Cases[distalData,{Alternatives@@tfData[[All,1]],Alternatives@@tfData[[All,1]]}]];
		
		(*Add metadata to edges*)
		proximalEdges=Property[#,{"Network"->"Proximal",EdgeStyle->RGBColor[0.43266956588082706`,0.5137102311741817`,0.6462653543907836`]}]&/@proximalEdges;
		distalEdges=Property[#,{"Network"->"Distal",EdgeStyle->LightGray}]&/@distalEdges;
		
		(*Getting the vertices*)
		vertices=Table[
			Property[vertex[[1]],{"KnownType"->vertex[[2]],"AssignedType"->
			Switch[vertex[[2]],
				"+","+",
				"-","-",
				"+-","+-",
				"?",RandomChoice[{"+","-","+-"}]
			]}]
		,{vertex,tfData}];
		
		(*Color the vertices*)
		vertices=Table[
			Property[vertex[[1]],AppendTo[vertex[[2]],VertexStyle->Switch["AssignedType"/.vertex[[2]],
				"+",Green,
				"-",Red,
				"+-",Yellow
			]]]
		,{vertex,vertices}];
		
		(*Generate edge weights*)
		proximalWeights=Table[
			edge->Switch[Cases[vertices,Property[edge[[1,1]],{___,HoldPattern["AssignedType"->type_],___}]->type][[1]],
				"+",100,
				"-",-100,
				"+-",RandomChoice[{-100,100}]]
		,{edge,proximalEdges}];
		
		distalWeights=Table[
			edge->Switch[Cases[vertices,Property[edge[[1,1]],{___,HoldPattern["AssignedType"->type_],___}]->type][[1]],
				"+",100,
				"-",-100,
				"+-",RandomChoice[{-100,100}]]
		,{edge,distalEdges}];
		
		Switch[output,
			"full",(newEdges=DeleteCases[distalEdges,Property[Alternatives@@proximalEdges[[All,1]],_]];allEdges=Join[proximalEdges,newEdges];positions=Position[distalEdges,Property[Alternatives@@proximalEdges[[All,1]],_]];allWeights=DeleteDuplicates[Join[proximalWeights,Delete[distalWeights,positions]]];),
			"proximal",(allEdges=proximalEdges;allWeights=proximalWeights;),
			"distal",(allEdges=distalEdges;allWeights=distalWeights;)
		];
		
		(*Count number of positive and negative weights per vertex*)
		testWeights={#[[1,1,1]],#[[2]]}&/@allWeights;
		vertices=Table[
			Property[vertex[[1]],AppendTo[vertex[[2]],"nPositive"->Length[Cases[testWeights,{vertex[[1]],100}]]]]
		,{vertex,vertices}];
		
		vertices=Table[
			Property[vertex[[1]],AppendTo[vertex[[2]],"nNegative"->Length[Cases[testWeights,{vertex[[1]],-100}]]]]
		,{vertex,vertices}];
		
		(*Assemble the graph*)
		Graph[vertices,allEdges,EdgeWeight->allEdges/.allWeights,opts]
)]


weightDistGraph[graph_Graph,{actRatio_,reprRatio_,randRatio_},seed_Integer]:=
	Module[{vertices,edges,vertexMap,weightMap,i},(
		vertices=VertexList[graph];
		edges=EdgeList[graph];
		SeedRandom[seed];

		vertexMap=Table[
			vertices[[i]]->RandomChoice[{actRatio,reprRatio,randRatio}->{100,-100,Hold[RandomChoice[{100,-100}]]}]
		,{i,1,Length[vertices]}];

		weightMap=Table[
			ReleaseHold[edges[[i]]->Evaluate[edges[[i,1]]/.vertexMap]]
		,{i,1,Length[edges]}];

		Graph[vertices,edges,EdgeWeight->edges/.weightMap]
	)]

getSelfLoops[graph_Graph]:=
	Module[{weightMap,x,w},(
		weightMap=getWeightMap[graph];
		Cases[weightMap,HoldPattern[x_\[DirectedEdge]x_->w_]->{x,w}]
	)]

countLooseNodes[graph_Graph]:=
	Module[{vertices,edges,normal,sl},(
		vertices=VertexList[graph];
		edges=EdgeList[graph];
		normal=Length[Cases[VertexDegree[graph],0]];
		sl=0;
		
		Table[
			If[VertexDegree[graph,vertex]==2,
				If[Length[Cases[edges,vertex\[DirectedEdge]vertex]]>0,
					sl+=1;
				];
			];
		,{vertex,vertices}];
		normal+sl
	)]

getLooseNodes[graph_Graph]:=
	Module[{vertices,edges,result},(
		vertices=VertexList[graph];
		edges=EdgeList[graph];
		
		result={};

		Table[
			If[VertexDegree[graph,vertex]==0,AppendTo[result,vertex]];
			If[VertexDegree[graph,vertex]==2,
				If[Length[Cases[edges,vertex\[DirectedEdge]vertex]]>0,AppendTo[result,vertex]]];
		,{vertex,vertices}];
		
		DeleteCases[result,Null]
	)]

removeSL[graph_Graph,seed_Integer]:=
	Module[{vertices,weightMap,edges,selfLoopVertices,v,succes,testVertex,testEdge,selfLoopWeight,testEdgeWeight},(
		SeedRandom[seed];
		vertices=VertexList[graph];
		weightMap=getWeightMap[graph];
		edges=EdgeList[graph];
		
		selfLoopVertices=Cases[edges,v_\[DirectedEdge]v_->v];

		(*Process all selfLoopVertices*)
		Table[
			
			succes=False;

			While[succes==False,
				testVertex=RandomChoice[vertices];

				(*Check if we have picked a valid testVertex to shift edges with*)
				If[testVertex!=v&&VertexInDegree[graph,testVertex]>0&&Length[Cases[edges,v\[DirectedEdge]testVertex]]==0,
					
					(*Pick one of the edges going into the testVertex to shift it to v*)
					testEdge=RandomChoice[Cases[edges,_\[DirectedEdge]testVertex]];
					
					(*Check if the new edge we want to make already exists and if we haven't picked another self loop*)
					If[Length[Cases[edges,testEdge[[1]]\[DirectedEdge]v]]==0 &&testEdge[[1]]!=testEdge[[2]],
						
						(*Add the new edges*)
						AppendTo[edges,v\[DirectedEdge]testVertex];
						AppendTo[edges,testEdge[[1]]\[DirectedEdge]v];
						
						(*Remove the old edges*)
						edges=DeleteCases[edges,v\[DirectedEdge]v];
						edges=DeleteCases[edges,testEdge];
						
						(*Alter the weightMap*)
						selfLoopWeight=v\[DirectedEdge]v/.weightMap;
						testEdgeWeight=testEdge/.weightMap;
						
						AppendTo[weightMap,v\[DirectedEdge]testVertex->selfLoopWeight];
						AppendTo[weightMap,testEdge[[1]]\[DirectedEdge]v->testEdgeWeight];
						weightMap=DeleteCases[weightMap,v\[DirectedEdge]v->_];
						weightMap=DeleteCases[weightMap,testEdge->_];
						
						(*Set the succes flag*)
						succes=True;
					];
				];
			];
			,{v,selfLoopVertices}
		];
		
		Graph[vertices,edges,EdgeWeight->edges/.weightMap]
		
	)]

bruteRemoveSL[graph_Graph]:=
	Module[{vertices,edges,weightMap},(
		vertices=VertexList[graph];
		edges=EdgeList[graph];
		weightMap=getWeightMap[graph];
		
		edges=DeleteCases[edges,x_\[DirectedEdge]x_];
		weightMap=DeleteCases[weightMap,x_\[DirectedEdge]x_->_];
		
		Graph[vertices,edges,EdgeWeight->edges/.weightMap]
	)]

addSL[graph_Graph,number_Integer,seed_Integer]:=
	Module[{weightMap,vertices,edges,i,testVertex,randomInEdge,randomOutEdge,inWeight,outWeight},(
		SeedRandom[seed];
		weightMap=getWeightMap[graph];
		vertices=VertexList[graph];
		edges=EdgeList[graph];
		i=0;

		While[i<number,
			testVertex=RandomChoice[vertices];

			If[VertexInDegree[graph,testVertex]>0&&VertexOutDegree[graph,testVertex]>0&&Length[Cases[edges,testVertex\[DirectedEdge]testVertex]]==0,
				(*We have found a suitable vertex, let's create the selfloop.*)
				
				(*Pick the edges we are going to change*)
				randomInEdge=RandomChoice[Cases[edges,_\[DirectedEdge]testVertex]];
				randomOutEdge=RandomChoice[Cases[edges,testVertex\[DirectedEdge]_]];
				
				(*Check if we have suitable random edges*)
				If[randomInEdge[[1]]!=randomOutEdge[[2]],
					
					(*Check if the new edge already exists*)
					If[Length[Cases[edges,randomInEdge[[1]]\[DirectedEdge]randomOutEdge[[2]]]]==0,
						
						(*Add the new edges*)
						AppendTo[edges,randomInEdge[[1]]\[DirectedEdge]randomOutEdge[[2]]];
						AppendTo[edges,randomOutEdge[[1]]\[DirectedEdge]randomInEdge[[2]]];
						
						(*Remove the old edges*)
						edges=DeleteCases[edges,randomInEdge];
						edges=DeleteCases[edges,randomOutEdge];
						
						(*Alter the weightMap*)
						inWeight=randomInEdge/.weightMap;
						outWeight=randomOutEdge/.weightMap;
						
						AppendTo[weightMap,randomInEdge[[1]]\[DirectedEdge]randomOutEdge[[2]]->inWeight];
						AppendTo[weightMap,randomOutEdge[[1]]\[DirectedEdge]randomInEdge[[2]]->outWeight];
						weightMap=DeleteCases[weightMap,randomInEdge->_];
						weightMap=DeleteCases[weightMap,randomOutEdge->_];
						
						i++
					];
				];
			];
		];
	
	Graph[vertices,edges,EdgeWeight->edges/.weightMap]
	
	)]

addSpecificSL[graph_Graph,vertex_,weight_Integer]:=
	Module[{vertices,edges,weightMap},
		vertices=VertexList[graph];
		edges=EdgeList[graph];
		weightMap=getWeightMap[graph];
		
		(*Check if vertex already has a self loop*)
		If[Length[Cases[edges,vertex\[DirectedEdge]vertex,\[Infinity]]]==0,
			
			AppendTo[edges,vertex\[DirectedEdge]vertex];
			AppendTo[weightMap,vertex\[DirectedEdge]vertex->weight];
			
		];
		
		Graph[vertices,edges,EdgeWeight->edges/.weightMap]
	]

addRandomEdges[graph_Graph,nNodes_Integer,seed_Integer]:=
	Module[{newGraph,vertices,weights,i,newEdge,edges,weightMap},(
		SeedRandom[seed];
		newGraph=graph;
		vertices=VertexList[graph];
		weights=Union[getWeightMap[graph][[All,2]]];
		i=0;
		
		While[i<nNodes,
			newEdge=RandomChoice[vertices]\[DirectedEdge]RandomChoice[vertices];
			If[Length[Cases[EdgeList[newGraph],newEdge]]==0&&newEdge[[1]]!=newEdge[[2]],
				edges=Append[EdgeList[newGraph],newEdge];
				weightMap=Append[getWeightMap[newGraph],newEdge->RandomChoice[weights]];
				
				newGraph=Graph[vertices,edges,EdgeWeight->edges/.weightMap];
				i++;
			];
		];
		
		newGraph
	)]

getWeightMap[graph_Graph]:=Map[#->PropertyValue[{graph,#},EdgeWeight]&,EdgeList[graph]];

randomIOGraph[graph_Graph,max_Integer,interval_Integer,seed_Integer,keepSelfLoops_Symbol]:=
	Module[{newGraph,weightMap,testEdges,newMaps,out,i,result,dSLCounter},(
		result={};
		newGraph=graph;
		weightMap=getWeightMap[graph];
		i=0;
		dSLCounter=0;
		SeedRandom[seed];

		While[i<=max,
			testEdges=Table[RandomChoice[EdgeList[newGraph]],{2}];
			If[
				Length[EdgeList[newGraph,testEdges[[1,1]]\[DirectedEdge]testEdges[[2,2]]]]==Length[EdgeList[newGraph,testEdges[[2,1]]\[DirectedEdge]testEdges[[1,2]]]]==0 && Length[Union[testEdges]]==2 &&
					If[keepSelfLoops,If[testEdges[[1,1]]!=testEdges[[1,2]] && testEdges[[2,1]]!=testEdges[[2,2]],True,False],True] &&
					If[Length[Cases[testEdges,x_\[DirectedEdge]x_]]==2,dSLCounter++;False,True],

				newMaps={testEdges[[1,1]]\[DirectedEdge]testEdges[[2,2]]->testEdges[[1]]/.weightMap,testEdges[[2,1]]\[DirectedEdge]testEdges[[1,2]]->testEdges[[2]]/.weightMap};

				weightMap=DeleteCases[weightMap,testEdges[[1]]->_];
				weightMap=DeleteCases[weightMap,testEdges[[2]]->_];
				weightMap=Join[weightMap,newMaps];

				newGraph=EdgeDelete[newGraph,testEdges];
				newGraph=EdgeAdd[newGraph,{testEdges[[1,1]]\[DirectedEdge]testEdges[[2,2]],testEdges[[2,1]]\[DirectedEdge]testEdges[[1,2]]}];
				i++;
				If[Mod[i,interval]==0,
					AppendTo[result,Graph[VertexList[newGraph],EdgeList[newGraph],EdgeWeight->EdgeList[newGraph]/.weightMap]];
				];
			];
		];
		(*Print[dSLCounter];*)
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
		(*Print[dSLCounter];*)
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
	Module[{},(
		If[VertexDegree[graph,v]>0,
			(VertexOutDegree[graph,v]-VertexInDegree[graph,v])/(VertexOutDegree[graph,v]+VertexInDegree[graph,v]),
			0
		]
	)]

degreeHistogram[graph_Graph,type_String,opts:OptionsPattern[]]:=
	Module[{function,data,sl,rest,max},(
		Switch[type,
			"In",function=VertexInDegree,
			"Out",function=VertexOutDegree,
			"All",function=VertexDegree
		];

		sl=getSelfLoops[graph][[All,1]];
		rest=Select[VertexList[graph],!MemberQ[sl,#]&];
		data={function[graph,#]&/@sl,function[graph,#]&/@rest};

		max=Max[Flatten[data]];

		Histogram[
			data,
			{-.5,max+1.5,1},
			opts,
			ChartLayout->"Stacked",
			AxesLabel->{"Degree","Frequency"},
			PlotLabel->Switch[type,
				"In","In degree",
				"Out","Out degree",
				"All", "Total degree"
			],
			ChartLegends->Placed[{"Selfloops","Normal"},Bottom]
		]
	)]

countSelfLoops[g_Graph]:=
	Module[{x},
		Cases[EdgeList[g],x_\[DirectedEdge]x_]//Length
	]

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

createAttractorGraph[attractorList_List]:=
	Module[{edges,beginPoint,endPoint},(
		edges=Table[
			beginPoint=i;
			endPoint=If[i+1>Length[attractorList],1,i+1];
			attractorList[[beginPoint]]\[DirectedEdge]attractorList[[endPoint]]
			,{i,1,Length[attractorList]}
		];
		Graph[Sort[edges]]
	)]

attractorHistogram[symbols_List,opts:OptionsPattern[]]:=
	Module[{activeNodes,cycleSizes,nStates,result,nRuns,count},(
		count=Total[attractorCount/@symbols];
		activeNodes=activeNodeCount/@symbols//Flatten;
		cycleSizes=attractorLengths/@symbols//Flatten;
		nStates=Table[vertexCount[symbols[[1]]],{count}];
		nRuns=Length[symbols];

		result=activeNodes/cycleSizes/nStates;

		Histogram[
			result,
			{-0.005,1.005,0.01},
			opts,
			ImageSize->800,
			PlotLabel->ToString[NumberForm[count,DigitBlock->3,NumberSeparator->"."]]<>" attractors"<>If[nRuns>1," ("<>ToString[nRuns]<>" runs)",""],
			PlotRange->{{-0.005,1.0},Automatic},
			AxesOrigin->{-0.005,0}
		]
	)]

End[]


EndPackage[]
