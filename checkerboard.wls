#!/usr/bin/env wolframscript

imageFolder = "output";
exportToPov = False;
exportToPov = True;
epsilon = 0.000000001;
stepCount = 180;

faceIndicesTetra = Union[Flatten[Map[Subsets[#, {3}]&, {{1, 2, 3, 4}}], 1]];
edgeIndicesIcosa = Union[Flatten[Map[Subsets[#, {2}]&, {{1, 2, 3, 4}}], 1]];

limit = 3;

finalSize = True;
(* finalSize = False; *)

imageSize = If[finalSize,
        512,
        170
    ] * {4, 3};


outputFolder = FileNameJoin[{imageFolder, "tetrahedra_animation_frames_" <> If[finalSize, "final", "test"]}];
If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

cubeVertices[dimension_] := 2 * Map[IntegerDigits[#, 2][[2;;dimension+1]]&, Table[k, {k, 2^dimension, 2 * 2^dimension - 1}]] - 1;
allPlusMinus[v_] := Union[Map[(v #)&, cubeVertices[Length[v]]]];
rotated[vertices_, center_, axis_, angle_] := Map[# + center &, Map[# - center &, vertices].RotationMatrix[angle, axis]];

getEdges[vertices_] := Module[
    {edgeLength, i, j},
    edges = {};
    edgeLength = 10^10;
    For[i = 2, i < Length[vertices], i++, 
        If[ Norm[vertices[[i]] - vertices[[1]] * 1.0] < edgeLength + epsilon, edgeLength = Norm[vertices[[i]] - vertices[[1]]]];
    ];

    For[i = 1, i <= Length[vertices]-1, i++, 
        For[j = i + 1, j <= Length[vertices], j++, 
            If[ Abs[Norm[1.0 * vertices[[i]] - vertices[[j]]] - edgeLength] < epsilon, AppendTo[edges, {i, j}] ];   
        ];
    ];
    edges
];

cummulatedVerticesTetra = {{}, {}, {}, {}};
cummulatedEdgesTetra = {{}, {}, {}, {}, {}, {}};
viewRotationAngle = - Pi / 2.;

drawImage[tetrahedra_, progress_, serialNumber_] := Module[
    {thickness, range, lighting, graph, outputFileName, cropCommand, nodeIndex, edgeIndex, tetrahedron},
    (* progress: 0 to 1 *)
    (* viewpoint: from 10,0,0 to 0,10,0 *)
    (* viewPoint = {10, 0, 0} * 100;  *)
    (* viewPoint = {10, -3, 8} * 100; *)
    viewPoint = {
        1, 0,
        Sin[progress * Pi] * 0.5} * 30 * Cosh[5 * (progress - 0.5)];

    vertical = {0, 0, 1};

    thickness = 0.03;
    (* 5.65 for the best *)
    range = 5.65 * (1 + 0.35 * (1 - Cos[progress * Pi] ^ 30));
    lighting = {{"Point", White, {100, -100, 100}}};

    viewRotationAngle = progress * 3 Pi / 2;
    rotationMatrix = RotationMatrix[viewRotationAngle, vertical];

    graphElements = {};
    faceGrayLevel = 0.8 * (1 - Cos[progress * Pi] ^ 30) + epsilon;
    edgeGrayLevel = 0.2;
    faceColor = GrayLevel[faceGrayLevel];
    edgeColor = GrayLevel[edgeGrayLevel];

    For[tetraIndex = 1, tetraIndex <= Length[tetrahedra], tetraIndex++,
        tetrahedron = tetrahedra[[tetraIndex]];
        graphElements = Join[graphElements,
            Map[{FaceForm[faceColor], Polygon[tetrahedron[[faceIndicesTetra[[#]]]].rotationMatrix]}&, Range[Length[faceIndicesTetra]]]
        ];

        If[faceGrayLevel > edgeGrayLevel,
            graphElements = Join[graphElements, Map[{FaceForm[edgeColor], Cylinder[tetrahedron[[#]].rotationMatrix, thickness]}&, edgeIndicesIcosa]];
        ];
    ];

    graph = Graphics3D[
        graphElements,
        ViewPoint -> viewPoint,
        ViewVertical -> vertical,
        PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
        Lighting -> lighting
    ];

    outputFileName = "frame" <> IntegerString[serialNumber, 10, 3] <> ".png";
    outputFileName = FileNameJoin[{outputFolder, outputFileName}];

    cropCommand = "mogrify -background white -alpha remove -alpha off -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

    resizeCommand = "magick convert " <> outputFileName <> " -resize 512x512 -filter box " <> outputFileName;

    recolorCommand = "magick convert " <> outputFileName <> " -set colorspace Gray -separate -average " <> outputFileName;


    If[exportToPov,
        If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
        POVRayRender[graph, 
            Method -> "Triangle", 
            OutputPath -> outputFileName, 
            ImageSize -> imageSize
            , RenderOptions -> "+A0.001 -J +UA"
        ];
        Run[cropCommand];
        Run[recolorCommand];
        If[finalSize, Run[resizeCommand]];
        ,
        Export[outputFileName, graph];
    ];
];



exportAnimation[] := Module[{},
    gifFileName = "tetrahedra_" <> If[finalSize, "final", "test"];
    gifFolder = imageFolder;
    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[Floor[360/stepCount]] <> " -dispose 2 " <> outputFolder <> "\\*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

    mp4Command = "ffmpeg -i " <> gifFolder <> "\\" <> gifFileName <> ".gif" 
        <> " -movflags faststart -pix_fmt yuv420p "
        <> gifFolder <> "\\" <> gifFileName <> ".mp4";

    Print[gifCommand];
    Run[gifCommand];
    Print["Saved the animation to " <> gifFolder <> "\\" <> gifFileName <> ".gif"];
];



normalizedInnerProd[u_, v_] := u.v/Norm[u]/Norm[v];
getLattice[vectors_, limit_] := Flatten[
    Table[
        {i, j, k}.vectors, {i, -limit, limit}, {j, -limit, limit}, {k, -limit, limit}
    ], 2];
getDiamondLattice[vectors_, limit_] := Module[
    {baseLattice, offsetLattice},
    baseLattice = getLattice[vectors, limit];
    offsetLattice = Map[# + Total[vectors]/4 &, baseLattice];
    Union[baseLattice, offsetLattice]
];
baseVectors = {
    {0, 1, 1},
    {1, 0, 1},
    {1, 1, 0}
} * 4;

baseLattice = getDiamondLattice[baseVectors, limit];
offset = {1, 1, 1};
baseLattice = Map[# + offset&, baseLattice];
diamondEdges = getEdges[baseLattice];
diamondEdges = Union[diamondEdges, Map[Reverse, diamondEdges]];

tetrahedra = {};
cutoff = 9;
For[vertexIndex = 1, vertexIndex <= Length[baseLattice], vertexIndex++,
    incidentEdges = Select[diamondEdges, #[[1]] == vertexIndex&];
    tetrahedron = Map[Mean[baseLattice[[#]]]&, incidentEdges];
    If[Length[tetrahedron] == 4 && Max[Abs[tetrahedron]] <= cutoff && Norm[Mean[tetrahedron]] < cutoff * 1.2,
        AppendTo[tetrahedra, tetrahedron];
    ]
];

serialNumber = 0;
For[serialNumber = 0, serialNumber <= stepCount - 1, serialNumber++,
    progress = (1 - Cos[Pi*(1/(stepCount - 1) * serialNumber)])/2;
    Print[{serialNumber, stepCount, progress * 1.0}];
    drawImage[tetrahedra, progress, serialNumber];
];

exportAnimation[];



