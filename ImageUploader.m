(* ::Package:: *)

Begin["SEUploader`"];

With[{lversion = Import["version", "Text"]},

Global`palette = PaletteNotebook[DynamicModule[{},
   
   Column[{
   	 Tooltip[
      Button["Upload to SE",
       With[{img = rasterizeSelection1[]}, 
        If[img === $Failed, Beep[], uploadWithPreview[img]]],
       Appearance -> "Palette"],
       "Upload the selected expression as an image to StackExchange", TooltipDelay -> Automatic],
     
     If[$OperatingSystem === "Windows",
      
      Tooltip[
       Button["Upload to SE (pp)",
        With[{img = rasterizeSelection2[]}, 
         If[img === $Failed, Beep[], uploadWithPreview[img]]],
        Appearance -> "Palette"],
      "Upload the selected experssion as an image to StackExchange\n(pixel-perfect rasterization)", TooltipDelay -> Automatic],
      
      Unevaluated@Sequence[]
      ],

      Tooltip[
      	Button["History...", historyDialog[], Appearance -> "Palette"],
      	"See previously uploaded images and copy their URLs", TooltipDelay -> Automatic],
      	
      Tooltip[
      	Button["Update check...", checkForUpdate[], 
      		Appearance -> "Palette",
      		Background -> Dynamic@If[CurrentValue[$FrontEnd, {TaggingRules, "SEUploaderVersion"}]  =!= version, LightRed, Automatic]
      	],
      	"Check for newer versions of the uploader palette", TooltipDelay -> Automatic] 
 
     }],
   
   (* init start *)
   Initialization :>
    (
     (* always refers to the palette notebook *)
     pnb = EvaluationNotebook[];
          
     (* VERSION CHECK CODE *)
     
     (* the palette version *)
     version = lversion;
     
     (* check the latest version on GitHub *)
     checkOnlineVersion[] := 
      Module[{onlineVersion},
      	Quiet@Check[
      		onlineVersion = Import["https://raw.github.com/szhorvat/SEUploader/master/version"],
      		Return[$Failed]
      	];
      	CurrentValue[$FrontEnd, {TaggingRules, "SEUploaderVersion"}] = onlineVersion
      ];
      
     checkForUpdate[] :=
      Module[{},
      	If[checkOnlineVersion[] =!= $Failed,
	      	MessageDialog[StringForm["Online version: `1`\nInstalled version: `2`",
	      	 CurrentValue[$FrontEnd, {TaggingRules, "SEUploaderVersion"}],
	      	 version],
	      	 WindowTitle -> "Version information"],
	     
	     	MessageDialog[
	     	 StringForm["Update check failed. Please check your internet connection.\nInstalled version: ``", version],
	     	 WindowTitle -> "Version information"
	     	] 	 
      	]
      ];
    
     (* IMAGE UPLOAD CODE *)
     
     (* stackImage uploads an image to SE and returns the image URL *)
     
     stackImage::httperr = "Server returned respose code: `1`";
     stackImage::err = "Server returner error: `1`";
     
     stackImage[g_] :=
      Module[
       {getVal, url, client, method, data, partSource, part, entity, 
        code, response, error, result},
       
       getVal[res_, key_String] :=
        With[{k = "var " <> key <> " = "},
         StringTrim[
          
          First@StringCases[
            First@Select[res, StringMatchQ[#, k ~~ ___] &], 
            k ~~ v___ ~~ ";" :> v],
          "'"]
         ];
       
       data = ExportString[g, "PNG"];
       
       JLink`JavaBlock[
        url = "http://stackoverflow.com/upload/image";
        client = JLink`JavaNew["org.apache.commons.httpclient.HttpClient"];
        method = JLink`JavaNew["org.apache.commons.httpclient.methods.PostMethod", url];
        partSource = JLink`JavaNew[
                        "org.apache.commons.httpclient.methods.multipart.ByteArrayPartSource", "mmagraphics.png", 
                        JLink`MakeJavaObject[data]@toCharArray[]];
        part = JLink`JavaNew["org.apache.commons.httpclient.methods.multipart.FilePart", "name", partSource];
        part@setContentType["image/png"];
        entity = JLink`JavaNew[
                    "org.apache.commons.httpclient.methods.multipart.MultipartRequestEntity", 
                    {part}, method@getParams[]];
        method@setRequestEntity[entity];
        code = client@executeMethod[method];
        response = method@getResponseBodyAsString[];
       ];
       
       If[code =!= 200, Message[stackImage::httperr, code]; Return[$Failed]];
       response = StringTrim /@ StringSplit[response, "\n"];
       
       error = getVal[response, "error"];
       result = getVal[response, "result"];
       If[StringMatchQ[result, "http*"],
        result,
        Message[stackImage::err, error]; $Failed]
       ];

     (* Copy text to the clipboard.  Works on v7. *)
     copyToClipboard[text_] := 
      Module[{nb},
       nb = NotebookCreate[Visible -> False];
       NotebookWrite[nb, Cell[text, "Text"]];
       SelectionMove[nb, All, Notebook];
       FrontEndTokenExecute[nb, "Copy"];
       NotebookClose[nb];
     ];
     
     historyDialog[] :=         
        MessageDialog[
          Column[{
          	Style["Click a thumbnail to copy its URL.", Bold],
            Grid@Partition[PadRight[
          	  Tooltip[Button[#1, copyToClipboard[#2], Appearance -> "Palette"], #2, TooltipDelay -> Automatic] & @@@ CurrentValue[pnb, {TaggingRules, "ImageUploadHistory"}], 
           	  9, ""], 3]
          }], 
          WindowTitle -> "History", WindowSize -> {450, All}];

     uploadButtonAction[img_] :=
        Module[
          {url, markdown},
          Check[
           url = stackImage[img],
           Return[]
          ];
          markdown = "![Mathematica graphics](" <> url <> ")";
          copyToClipboard[markdown];
          PrependTo[CurrentValue[pnb, {TaggingRules, "ImageUploadHistory"}], 
             {Thumbnail@Image[img], url}];
          If[Length[CurrentValue[pnb, {TaggingRules, "ImageUploadHistory"}]] > 9, 
             CurrentValue[pnb, {TaggingRules, "ImageUploadHistory"}] = Most@CurrentValue[pnb, {TaggingRules, "ImageUploadHistory"}]];
        ];
     
     (* returns available vertical screen space, 
     taking into account screen elements like the taskbar and menu *)
     screenHeight[] := -Subtract @@ 
        Part[ScreenRectangle /. Options[$FrontEnd, ScreenRectangle], 2];
     
     uploadWithPreview[img_Image] :=
      CreateDialog[
       Column[{
         Style["Upload image to StackExchange network?", Bold],
         Pane[
          Image[img, Magnification -> 1], {Automatic, 
           Min[screenHeight[] - 140, 1 + ImageDimensions[img][[2]]]},
          Scrollbars -> Automatic, AppearanceElements -> {}, 
          ImageMargins -> 0
          ],
         Item[ChoiceButtons[{"Upload and copy MarkDown"}, {uploadButtonAction[img]; DialogReturn[]}], Alignment -> Right]
         }],
       WindowTitle -> "Upload image to StackExchange?"
       ];
     
     (* Multiplatform, fixed-width version.  
     The default max width is 650 to fit StackExchange *)
     rasterizeSelection1[maxWidth_: 650] := 
      Module[{target, selection, image},
       selection = NotebookRead[SelectedNotebook[]];
       If[MemberQ[Hold[{}, $Failed, NotebookRead[$Failed]], selection],
        
        $Failed, (* there was nothing selected *)
        
        target = CreateDocument[{}, WindowSelected -> False, Visible -> False, WindowSize -> maxWidth];
        NotebookWrite[target, selection];
        image = Rasterize[target, "Image"];
        NotebookClose[target];
        image
        ]
       ];
     
     (* Windows-only pixel perfect version *)
     rasterizeSelection2[] :=
      If[
       MemberQ[Hold[{}, $Failed, NotebookRead[$Failed]], NotebookRead[SelectedNotebook[]]],
       
       $Failed, (* there was nothing selected *)
       
       Module[{tag},
        FrontEndExecute[FrontEndToken[FrontEnd`SelectedNotebook[], "CopySpecial", "MGF"]];
        Catch[
         NotebookGet@ClipboardNotebook[] /. 
          r_RasterBox :> 
           Block[{}, 
            Throw[Image[First[r], "Byte", ColorSpace -> "RGB"], tag] /;
              True];
         $Failed,
         tag
         ]
        ]
       ];
     ) 
   (* init end *)
   ],

   TaggingRules -> {"ImageUploadHistory" -> {}},
   WindowTitle -> "SE Uploader"
]

]

End[];
