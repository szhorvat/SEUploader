(* ::Package:: *)

Begin["SEUploader`"];

With[{lversion = Import["version", "Text"]},

Global`palette = PaletteNotebook[DynamicModule[{},

   Block[{$ContextPath}, Needs["JLink`"]];
   JLink`InstallJava[];
   
   Dynamic@Column[{
   	 Tooltip[
      Button["Upload to SE",
       uploadButton[],
       Appearance -> "Palette"],
       "Upload the selected expression as an image to StackExchange", TooltipDelay -> Automatic],
     
     If[$OperatingSystem === "Windows",
      
      Tooltip[
       Button["Upload to SE (pp)",
        uploadPPButton[],
        Appearance -> "Palette"],
      "Upload the selected experssion as an image to StackExchange\n(pixel-perfect rasterization)", TooltipDelay -> Automatic],
      
      Unevaluated@Sequence[]
      ],

      Tooltip[
      	Button["History...", historyButton[], Appearance -> "Palette"],
      	"See previously uploaded images and copy their URLs", TooltipDelay -> Automatic],
      	
      Tooltip[
      	Button["Update...", updateButton[], 
      		Appearance -> "Palette",
      		Background -> Dynamic@If[CurrentValue[$FrontEnd, {TaggingRules, "SEUploaderVersion"}, version]  =!= version, 
      			                      LightMagenta, 
      			                      Automatic]
      	],
      	"Check for newer versions of the uploader palette", TooltipDelay -> Automatic] 
 
     }],
   
   (* init start *)
   Initialization :>
    (
     (* always refers to the palette notebook *)
     pnb = EvaluationNotebook[];
     
     (* HELPER FUNCTIONS *)
     
     closeButton[] :=
       DefaultButton["Close", DialogReturn[], ImageSize -> CurrentValue["DefaultButtonSize"], ImageMargins -> {{2,2}, {10,10}}];

     (* VERSION CHECK CODE *)
     
     (* the palette version number, stored as a string *)
     version = lversion;
     
     (* Update URLs *)
     versionURL = "https://raw.github.com/szhorvat/SEUploader/master/version";
     paletteURL = "https://raw.github.com/szhorvat/SEUploader/master/SEUploaderLatest.nb";
     
     (* check the latest version on GitHub *)
     checkOnlineVersion[] := 
      Module[{onlineVersion},
      	Quiet@Check[
      		onlineVersion = Import[versionURL, "Text"],
      		Return[$Failed]
      	];
      	CurrentValue[$FrontEnd, {TaggingRules, "SEUploaderLastUpdateCheck"}] = AbsoluteTime[];
      	CurrentValue[$FrontEnd, {TaggingRules, "SEUploaderVersion"}] = onlineVersion
      ];
     
     (* Check for updates on initialization if last check was > 3 days ago.
        The check will time out after 6 seconds. *) 
     If[AbsoluteTime[] > 3*3600*24 + CurrentValue[$FrontEnd, {TaggingRules, "SEUploaderLastUpdateCheck"}, 0],
 		TimeConstrained[checkOnlineVersion[], 6]
     ];
     
     onlineUpdate[] :=
       Module[{paletteSource, paletteExpression, paletteFileName, paletteDirectory},
       	paletteSource = Import[paletteURL, "String"];
       	If[paletteSource === $Failed, Beep[]; Return[]];
       	
       	(* Validate notebook. If GitHub is down, we shouldn't replace a working palette
       	   with the contents of an error page. *)
       	Quiet@Check[paletteExpression = ImportString[paletteSource, {"Package", "HeldExpressions"}], Beep[]; Return[]];
       	If[Extract[paletteExpression, {1,1,0}] =!= Notebook, Beep[]; Return[]];
       	
       	paletteFileName = NotebookFileName[pnb];
       	paletteDirectory = NotebookDirectory[pnb];
       	NotebookClose[pnb];
       	Export[paletteFileName, paletteSource, "String"];
       	FrontEndExecute[FrontEnd`ResetMenusPacket[{Automatic, Automatic}]];
       	
       	(* Note: FileNameTake is necessary to preserve the "PalettesMenuSettings",
       	   which are tied to the file name as a string.  If using the full path,
       	   the Front End will think we're opening a different palette and
       	   will not apply the "PalettesMenuSettings" *)
       	FrontEndTokenExecute["OpenFromPalettesMenu", FileNameTake[paletteFileName]];
       ];
      
     updateButton[] :=
      Module[{res},
      	res = checkOnlineVersion[];
      	CreateDialog[
      	 Column[{
      	   StringForm["`1`\nInstalled version: `2`\n\n`3`",
      	    If[res =!= $Failed,
      	      "Online version: " <> ToString@CurrentValue[$FrontEnd, {TaggingRules, "SEUploaderVersion"}],
      	      "Update check failed.  Please check your internet connection."
      	    ],
      	    version,
      	    Row[{ 
      	    	Hyperlink["Open home page", "http://meta.mathematica.stackexchange.com/a/32/12"], 
      	    	" | ", 
      	    	Hyperlink["History of changes", "https://github.com/szhorvat/SEUploader/commits/master"]
      	    }]
      	   ],
      	 
      	   Pane[
      	   	If[res =!= $Failed 
      	   		&& CurrentValue[$FrontEnd, {TaggingRules, "SEUploaderVersion"}, version] =!= version
      	   		&& FileNameSplit@NotebookDirectory[pnb] === Join[FileNameSplit[$UserBaseDirectory], {"SystemFiles", "FrontEnd", "Palettes"}],
      	   		
      	   	  ChoiceButtons[{"Update to new version"}, {onlineUpdate[]; DialogReturn[]}],
      	   	  
      	   	  closeButton[]
      	   	],
      	   	ImageSize -> 340, 
      	   	Alignment -> Right]
      	 }],
      	 
      	 WindowTitle -> "Version information"]
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
       
     (* PALETTE BUTTON ACTIONS AND HELPER FUNCTIONS *)

     (* Copy text to the clipboard.  Works on v7. *)
     copyToClipboard[text_] := 
      Module[{nb},
       nb = NotebookCreate[Visible -> False];
       NotebookWrite[nb, Cell[text, "Text"]];
       SelectionMove[nb, All, Notebook];
       FrontEndTokenExecute[nb, "Copy"];
       NotebookClose[nb];
     ];
     
     historyButton[] :=         
        CreateDialog[
          Column[{
          	Style["Click a thumbnail to copy its URL.", Bold],
            Dynamic@Grid@Partition[PadRight[
          	  Tooltip[
          	  	Button[#1, copyToClipboard[#2]; DialogReturn[], Appearance -> "Palette"], 
          	  	#2, TooltipDelay -> Automatic] & @@@ 
          	  CurrentValue[pnb, {TaggingRules, "ImageUploadHistory"}, {}], 
           	  9, ""], 3],
           	Item[Row[{
           		Spacer[200],
           		Button["Clear all", CurrentValue[pnb, {TaggingRules, "ImageUploadHistory"}] = {}, ImageSize -> CurrentValue["DefaultButtonSize"]],
           		Spacer[10], 
           		closeButton[]}
           		], Alignment -> Right]
          }], 
          WindowTitle -> "History"];

	 uploadButton[] :=
	   With[{img = rasterizeSelection1[]},
        If[img === $Failed, Beep[], uploadWithPreview[img]]];

     uploadPPButton[] := 
       With[{img = rasterizeSelection2[]}, 
        If[img === $Failed, Beep[], uploadWithPreview[img]]];


     (* button from the upload dialog *)
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
