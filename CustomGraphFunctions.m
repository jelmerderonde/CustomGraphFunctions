(* ::Package:: *)

BeginPackage["CustomGraphFunctions`"];


ai::usage "ai[], gives a lists of mutants (and mediator) with load";
generateTopology::usage = "generateTopology[g] generates a list whose elements encode edges in directed graph g with weights";
topologyList::usage = "topologyList[g] generates output of directed graph g suitable for export to use with CNetwork.";
generateGraph::usage = "generateGraph[topology] returns a graph based on the topology part of a CNetwork result file.";
regenerateGraph::usage = "regenerateGraph[topology]returns a graph based on an imported topology file. (Import[\"file.txt\",\"Data\"]).";
encodeGraph::usage = "encodeGraph[seed,weights,omit] generates a directed graph from the ENCODE consortium and uses seed to generate random weights, picked from list w. Omit is an optional boolean that determines wheter the floating vertex will be deleted. Its default is True.";
advancedEncodeGraph::usage = "advancedEncodeGraph[seed,type] generates a directed graph from the ENCODE consortium and uses seed to generate unknown weights. Type is a string that determines which network to return. Options are: full, proximal and distal.";
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
hierarchyLevels::usage = "hierarchyLevels[g,n,s] gives a list of n hierarchy levels of graph g, by sshing to server s and executing a Matlab script.";
hierarchyHistogram::usage = "hierarchyHistogram[g,n,s] gives a histogram of n hierarchy levels of graph g, by sshing to server s and executing a Matlab script.";
degreeHistogram::usage = "degreeHistogram[graph, type, options] gives a histogram of the vertexdegree's of graph. Type can be \"In\", \"Out\" or \"All\".";
levelInteractions::usage = "levelInteractions[g,n,s] gives a level interaction matrix of n hierarchy levels of graph g, by sshing to server s and executing a Matlab script.";
resultsIndex::usage = "resultsIndex[inputdir] searches the directory inputdir for result files and displays the available run results as a table.";
prepareRun::usage = "prepareRun[inputdirs,parameters,nproc,outputdir] prepares a run in the outputdir with the files in inputdirs as input.";
advancedPrepareRun::usage = "advancedPrepareRun[inputDirs,parameters,nProc,initialStatesDir,outputDir] prepares a run in the outputdir with the files in inputdirs as input.";
domainSizesHistogram::usage = "domainSizesHistogram[data] returns a log log histogram of domain sizes.";
getAttractorProfile::usage = "getAttractorProfile[resultsymbol] returns the attractorProfile of a result symbol.";
attractorHistogram::usage = "attractorHistogram[symbols,opts] returns a histogram with the ratio of active nodes on horizontal axis and the frequency of attractors on vertical. Only accepts a list of 1 or more result symbols.";
createAttractorGraph::usage ="createAttractorGraph[attractor] creates an attractor graph from attractor strings saved in a result.";

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
updateMethod::usage = "updateMethod[result] retursn the update method used to create the output file.";
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

ai[]:=
	Module[{data,hostname,cleandata,list,table},(
		hostname=If[$MachineName=="mediator","mediator","binftunnel"];
		data=Import["!ssh "<>hostname<>" ai","Table"];
		list=Table["mutant"<>ToString[i],{i,1,40}]; AppendTo[list,"mediator"];
		cleandata=Cases[data,{_,load_,nproc_,name_,_,_,_,user_}->{ToExpression[load]/ToExpression[nproc],name,ToExpression[load],ToExpression[nproc],user}];
		cleandata=Sort[Cases[cleandata,{_,Alternatives@@list,_,_,_}]];
		
		table=Table[
			{cleandata[[i,2]],ProgressIndicator[cleandata[[i,3]],{0,cleandata[[i,4]]}],If[IntegerPart[cleandata[[i,4]]-cleandata[[i,3]]]<0,0,IntegerPart[cleandata[[i,4]]-cleandata[[i,3]]]],cleandata[[i,5]]}
		,{i,1,Length[cleandata]}];

		PrependTo[table,{"Computer","Load","Free","User"}];
		Grid[table,Frame->All]
	)]

generateTopology[graph_Graph]:=
	Module[{weightMap,translationRules},(
		weightMap=getWeightMap[graph];
		translationRules=Rule@@@Partition[Riffle[VertexList[graph],Range[VertexCount[graph]]],2];
		
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

encodeGraph[seed_Integer,w_List,omit_Symbol:True] :=
	Module[{encodeData,prefix,tfs,encodeEdges,weights,g},(
		SeedRandom[seed];
		weights=Table[RandomChoice[w],{323}];
		prefix=If[$MachineName=="mediator","~/Project/Code/Mathematica/","/Users/jelmerderonde/Documents/Code/CNetwork/NETWORKS/ENCODE/"];
		
		encodeData=Import[prefix<>"enets2.Proximal_filtered.txt",{"Text","Words"}];
		tfs=Import[prefix<>"tfs.txt",{"Text","Words"}];
			
		encodeEdges=Cases[Partition[encodeData,2],{Apply[Alternatives,tfs],Apply[Alternatives,tfs]}];
		
		g=Graph[Apply[DirectedEdge,encodeEdges,2],EdgeWeight->weights];
		If[omit,
			VertexDelete[g,"ESRRA"],
			g
		]
	)]

advancedEncodeGraph[seed_Integer,output_String:"proximal",opts:OptionsPattern[]]:=
	Module[{prefix,proximalData,distalData,tfData,proximalEdges,distalEdges,vertices,proximalWeights,distalWeights,vertex,edge,type,newEdges,allEdges,allWeights,testWeights,positions},(
		SeedRandom[seed];
		prefix=Switch[$MachineName,
			"jderonde","/Users/jelmerderonde/Documents/01 - Active Projects/Research project 2/Data/ENCODE/",
			"mediator","~/Project/Code/Mathematica/",
			_,"/linuxhome/tmp/jelmer/Mathematica/"
		];
		
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
		
		Partition[Riffle[VertexList[graph],levels],2]
	)]

hierarchyHistogram[graph_Graph,nlevels_Integer,server_String,opts:OptionsPattern[]]:=
	Module[{colors,levels,data,h,v,l,x,i,j,hDegrees,slIndeces,slVertices,binLists,slBinList,slLevelVertices,slLevelList,labeler},(
		colors={Green,Blue,Red,Purple,Orange,Cyan};
		levels=hierarchyLevels[graph,nlevels,server][[All,2]];
		data=Table[
			Cases[
				Table[
					If[VertexDegree[graph][[v]]>0,
						{hDegree[graph,VertexList[graph][[v]]],levels[[v]]}
					]
				,{v,1,VertexCount[graph]}]
			,{h_,l}->h],
			{l,Range[Max[levels]]}
		];

		(*Counting the self loops*)
		hDegrees=hDegree[graph,#]&/@VertexList[graph];
		slIndeces=VertexIndex[graph,#]&/@Cases[EdgeList[graph],x_\[DirectedEdge]x_->x];
		slVertices=Table[
			{
				hDegrees[[i]],
				If[MemberQ[slIndeces,i],1,0]
			}
		,{i,1,VertexCount[graph]}];
		binLists=BinLists[slVertices[[All,1]],{-1.1+.2/3,1.1-.2/3,.2/3}];
		slBinList=Total/@DeleteCases[
			Table[
				Table[
					If[MemberQ[binLists[[i]],slVertices[[j,1]]],slVertices[[j,2]]]
				,{j,1,Length[slVertices]}]
			,{i,1,Length[binLists]}],Null,\[Infinity]];

		slLevelVertices=Table[
			{
				levels[[i]],
				If[MemberQ[slIndeces,i],1,0]
			}
		,{i,1,VertexCount[graph]}];
		slLevelList=Total/@DeleteCases[Table[
			Table[
				If[slLevelVertices[[j,1]]==i,slLevelVertices[[j,2]]]
			,{j,1,Length[slLevelVertices]}]
		,{i,Range[Max[levels]]}],Null,\[Infinity]];

		labeler[v_,{i_,j_},{ri_,cj_}]:=Placed[Join[ri,cj,{If[slBinList[[j]]>0&&i==Max[levels],slBinList[[j]],Null]}],Above,Column];

		Histogram[
			data,
			{-1.1+.2/3,1.1-.2/3,.2/3},
			opts,
			PlotRange->{Automatic,{0,30}},
			ChartStyle->colors,
			ChartLayout->"Stacked",
			ChartLegends->Table["Level "<>ToString[i]<>" ("<>ToString[slLevelList[[i]]]<> " SL's)"<>If[i==1," (bottom)",If[i==Max[levels]," (top)",""]],{i,Range[Max[levels]]}],
			LabelingFunction->labeler,
			PlotLabel->"# Selfloops: "<>StringJoin[Table[ToString[slLevelList[[i]]]<>If[i==Length[slLevelList],""," | "],{i,1,Length[slLevelList]}]]
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
				"screen -d -m ./"<>ToString[i]<>".sh"
			,{i,1,Length[runs]}]
		,"Text"];
		
)]

advancedPrepareRun[inputDirs_List,parameters_List,nProc_Integer,initialStatesDir_String,outputDir_String]:=
	Module[{inputFiles,networkSizes,nInitialStates,newStates,newState,commands},(
		SetDirectory[outputDir];
		DeleteFile[FileNames["*.*"]];
		
		(*Gather all inputfiles*)
		inputFiles={};
		Table[
			AppendTo[inputFiles,FileNames["*.txt",inputDir]];
		,{inputDir,inputDirs}];
		inputFiles=Flatten[inputFiles];
		
		(*Copy all the files*)
		Table[
			CopyFile[file,FileNameJoin[{Directory[],FileNameTake[file]}]];
		,{file,inputFiles}];
		
		(*Get network sizes*)
		inputFiles=Table[
			{file,ToExpression[Import[file,"Lines"][[1]]]}
		,{file,FileNames["*.txt"]}];
		networkSizes=Union[inputFiles[[All,2]]];
		
		(*Check if we have initial states files for all inputfiles*)
		SetDirectory[initialStatesDir];
		
		nInitialStates=Length[Import[FileNames["*.txt"][[1]],"Lines"]];
		If[nInitialStates==0,Print["No initial states files"];Abort[];];
		Table[
			If[!FileExistsQ[ToString[size]<>".txt"],
				(*Generate new file*)
				SeedRandom[size];
				newStates={};
				While[Length[newStates]<nInitialStates,
					newState=StringJoin[ToString/@Table[RandomChoice[{0,1}],{size}]];
					If[!MemberQ[newStates,newState],AppendTo[newStates,newState]];
				]
				Export[ToString[size]<>".txt",newStates,"Table"];
			],{size,networkSizes}];
		
		(*Build the commands*)
		SetDirectory[outputDir];
		
		commands=Flatten[Table[
			Table[
				"./main --input "<>file[[1]]<> " "<>parameter<>" --same-states "<>ToString[file[[2]]]<>".txt"
			,{parameter,parameters}]
		,{file,inputFiles}]];
		
		(*Partition commands*)
		commands=Partition[commands,IntegerPart[Length[commands]/nProc],IntegerPart[Length[commands]/nProc],1,{}];
		
		(*Build the run files*)
		Table[
			Export[ToString[i]<>".sh",commands[[i]],"Text"]
		,{i,1,Length[commands]}];
		
		Export["run.sh",
			Table[
				"screen -d -m ./"<>ToString[file]
		,{file,FileNames["*.sh"]}],"Text"];
		
		(*Copy the initial states files*)
		SetDirectory[initialStatesDir];
		Table[
			CopyFile[ToString[size]<>".txt",FileNameJoin[{outputDir,ToString[size]<>".txt"}]];
		,{size,networkSizes}];
		
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
		rawdata=DeleteCases[rawdata,If[attrout==1,{0,{"0"},0,{0},0},{0,0,{0},0}],\[Infinity]];

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
		updateMethod[name]^=Switch[rawdata[[2,10]],0,"sum>0",1,"sum>=0",_,"?"];
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
	(
	k=0;
	total=FileNames[FileNameJoin[{inputdir,"result*"}]]//Length;
	Monitor[
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
					k++;
				,{j,1,Length[files]}]
			,{i,1,Length[datasets]}];
			
			SetDirectory[initialdirectory];
			symbolscreated
		)],
	
		Column[{Row[{k,"/",total}," "],Row[{ProgressIndicator[k,{0,total}],k/total*100//N,"%"}," "]}]
	])

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
