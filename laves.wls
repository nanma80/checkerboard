#!/usr/bin/env wolframscript

imageFolder = "output";
exportToPov = False;
exportToPov = True;
epsilon = 0.000000001;
stepCount = 180;

faceIndicesTetra = Flatten[Table[{k + 1, Mod[k + 1, 6] + 1, m}, {k, 0, 5}, {m, 7, 8}], 1];
edgeIndicesIcosa = Join[Table[{k + 1, Mod[k + 1, 6] + 1}, {k, 0, 5}],
    Table[{k + 1, 7}, {k, 0, 5}],
    Table[{k + 1, 8}, {k, 0, 5}]
];

limit = 3;

finalSize = True;
(* finalSize = False; *)

imageSize = If[finalSize,
        512,
        170
    ] * {4, 3};


outputFolder = FileNameJoin[{imageFolder, "laves_animation_frames_" <> If[finalSize, "final", "test"]}];
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

drawImage[bipyramids_, progress_, serialNumber_] := Module[
    {thickness, range, lighting, graph, outputFileName, cropCommand, nodeIndex, edgeIndex, bipyramid},
    (* progress: 0 to 1 *)
    (* viewpoint: from 10,0,0 to 0,10,0 *)
    (* viewPoint = {10, 0, 0} * 100;  *)
    (* viewPoint = {10, -3, 8} * 100; *)
    viewPoint = {
        1, -1,
        Sin[progress * Pi] * Sqrt[2]} * 20 * Cosh[8 * (progress - 0.5)];

    vertical = {0, 0, 1};

    thickness = 0.03;
    (* 5.65 for the best *)
    (* range = 5.65 * (1 + 0.35 * (1 - Cos[progress * Pi] ^ 30)); *)
    range = 11.33 * (1 + 0.35 * (1 - Cos[progress * Pi] ^ 30));

    lighting = {{"Point", White, {100, -100, 100}}};

    viewRotationAngle = Pi/4 + Pi - Sin[progress * 2 * Pi] * Pi /4;
    rotationMatrix = RotationMatrix[viewRotationAngle, vertical];

    graphElements = {};
    faceGrayLevel = 0.8 * (1 - Cos[progress * Pi] ^ 30) + epsilon;
    edgeGrayLevel = 0.2;
    faceColor = GrayLevel[faceGrayLevel];
    edgeColor = GrayLevel[edgeGrayLevel];

    For[tetraIndex = 1, tetraIndex <= Length[bipyramids], tetraIndex++,
        bipyramid = bipyramids[[tetraIndex]];
        graphElements = Join[graphElements,
            Map[{FaceForm[faceColor], Polygon[bipyramid[[faceIndicesTetra[[#]]]].rotationMatrix]}&, Range[Length[faceIndicesTetra]]]
        ];

        If[faceGrayLevel > edgeGrayLevel,
            graphElements = Join[graphElements, Map[{FaceForm[edgeColor], Cylinder[bipyramid[[#]].rotationMatrix, thickness]}&, edgeIndicesIcosa]];
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
    gifFileName = "laves_" <> If[finalSize, "final", "test"];
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


rotate3[v_] := {v[[2]], v[[3]], v[[1]]};

rotate2[v_] := {v[[1]], -v[[2]], -v[[3]]};

applyOffset[points_, offset_] := Map[# + offset&, points];

rotate2offset[v_, offset_] := rotate2[v] + 2 * offset;

hexagon = {
    {0, -1, 1},
    {-1, 0, 1},
    {-1, 1, 0},
    {0, 1, -1},
    {1, 0, -1},
    {1, -1, 0}
};
poles = {{1, 1, 1}, {-1, -1, -1}};
offset = {0, 1, -1};

bipyramid = Join[hexagon, poles];

bipyramids = { bipyramid };

cutoff = 18;
For[i = 1, i < 13, i++,
    rotate2collection = {};
    For[j = 1, j <= Length[bipyramids], j++,
        bipyramid = bipyramids[[j]];
        AppendTo[rotate2collection, Map[rotate2offset[#, offset]&, bipyramid]];
    ];
    bipyramids = Union[bipyramids, rotate2collection, SameTest -> (Total[#1] == Total[#2]&)];

    rotate3collection = {};
    For[j = 1, j <= Length[bipyramids], j++,
        bipyramid = bipyramids[[j]];
        AppendTo[rotate3collection, Map[rotate3[#]&, bipyramid]];
        AppendTo[rotate3collection, Map[rotate3[rotate3[#]]&, bipyramid]];
    ];
    bipyramids = Union[bipyramids, rotate3collection, SameTest -> (Total[#1] == Total[#2]&)];
    bipyramids = Select[bipyramids, Max[Abs[applyOffset[#, -offset]]] < cutoff && Norm[Mean[applyOffset[#, -offset]]] < cutoff * 1.2&];
    Print[{i, Length[bipyramids]}];
];

Print[Length[bipyramids]];

bipyramids = Map[applyOffset[#, - offset]&, bipyramids, {1}];

(* drawImage[bipyramids, 0.25, 0]; *)

serialNumber = 0;
For[serialNumber = 0, serialNumber <= stepCount - 1, serialNumber++,
    progress = (1 - Cos[Pi*(1/(stepCount - 1) * serialNumber)])/2;
    Print[{serialNumber, stepCount, progress * 1.0}];
    drawImage[bipyramids, progress, serialNumber];
];

exportAnimation[];
