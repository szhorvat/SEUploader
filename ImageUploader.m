(* ::Package:: *)

Begin["SEUploader`"];

Global`palette = PaletteNotebook[DynamicModule[{},
   
   Column[{
     Button["Upload to SE",
      With[{img = rasterizeSelection1[]}, 
       If[img === $Failed, Beep[], uploadWithPreview[img]]],
      Appearance -> "Palette"],
     
     If[$OperatingSystem === "Windows",
      
      Button["Upload to SE (pp)",
       With[{img = rasterizeSelection2[]}, 
        If[img === $Failed, Beep[], uploadWithPreview[img]]],
       Appearance -> "Palette"],
      
      Unevaluated@Sequence[]
      ],

      Button["History...", 
        MessageDialog[
          Column[{Style["Click an URL to copy it", Bold],
                  Grid@CurrentValue[pnb, {TaggingRules, "ImageUploadHistory"}]}], 
          WindowTitle -> "History", WindowSize -> {360, All}],
        Appearance -> "Palette"]
     }],
   
   (* init start *)
   Initialization :>
    (
     pnb = EvaluationNotebook[];
     
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


     copyToClipboard[text_] := 
      Module[{nb},
       nb = NotebookCreate[Visible -> False];
       NotebookWrite[nb, Cell[text, "Text"]];
       SelectionMove[nb, All, Notebook];
       FrontEndTokenExecute[nb, "Copy"];
       NotebookClose[nb];
     ];

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
             {Thumbnail@Image[img], Button[url, copyToClipboard[url], Appearance -> "Palette"]}];
          If[Length[CurrentValue[pnb, {TaggingRules, "ImageUploadHistory"}]] > 6, 
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

End[];
