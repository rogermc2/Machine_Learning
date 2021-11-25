--  Based on scikit-learn/sklearn/tree _export.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities;
with Criterion;
with Export_Types; use Export_Types;
with Node_Strings;
--  with Printing;
with Weights;

package body Graphviz_Exporter is

    procedure Head (Exporter    : DOT_Tree_Exporter;
                    Output_File : File_Type);
    procedure Recurse (Exporter    : in out DOT_Tree_Exporter;
                       Criteria    : Criterion.Classifier_Criteria_Type;
                       Output_File : File_Type; Depth : Natural := 0);
    procedure Tail (Exporter    : DOT_Tree_Exporter; Output_File : File_Type);

    --  -------------------------------------------------------------------------

    procedure C_Init (Exporter           : in out DOT_Tree_Exporter;
                      Output_File_Name   : Unbounded_String :=
                        To_Unbounded_String ("tree.dot");
                      Max_Depth          : Positive := Integer'Last;
                      Feature_Names      : Feature_Names_List :=
                        Unbounded_Package.Empty_Vector;
                      Class_Names        : Class_Names_List :=
                        Unbounded_Package.Empty_Vector;
                      Label              : Unbounded_String :=
                        To_Unbounded_String ("all");
                      Filled             : Boolean := False;
                      Leaves_Parallel    : Boolean := False;
                      Impurity           : Boolean := True;
                      Node_Ids           : Boolean := False;
                      Proportion         : Boolean := False;
                      Rotate             : Float := 0.0;
                      Rounded            : Boolean := False;
                      Special_Characters : Boolean := False;
                      Precision          : Positive := 3;
                      Font_Name          : Unbounded_String :=
                        To_Unbounded_String ("helvetica")) is
    begin
        Exporter.Output_File_Name := Output_File_Name;
        Exporter.Max_Depth := Max_Depth;
        Exporter.Feature_Names := Feature_Names;
        Exporter.Class_Names := Class_Names;
        Exporter.Label := Label;
        Exporter.Filled := Filled;
        Exporter.Leaves_Parallel := Leaves_Parallel;
        Exporter.Impurity := Impurity;
        Exporter.Node_Ids := Node_Ids;
        Exporter.Proportion := Proportion;
        Exporter.Rotate := Rotate;
        Exporter.Rounded := Rounded;
        Exporter.Special_Characters := Special_Characters;
        Exporter.Precision := Precision;
        Exporter.Font_Name := Font_Name;
        Exporter.Ranks := Export_Types.Export_Maps.Empty_Map;
        Exporter.Colours := Export_Types.Integer_Colours_Package.Empty_Vector;
        Exporter.Bounds  := Export_Types.Bounds_Package.Empty_Vector;
        Exporter.Initialized := False;

    end C_Init;

    --  -------------------------------------------------------------------------

    function Colour_Brew (Num_Colours : Positive)
                          return Integer_Colours_List is

        function Modulo (Dividend, Divisor : in Float) return Float is
            N : constant Integer := Integer (Float'Floor (Dividend / Divisor));
        begin
            return Dividend - Divisor * Float (N);
        end Modulo;

--          Routine_Name  : constant String :=
--                            "Graphviz_Exporter.Colour_Brew ";
        Saturation  : constant Float := 0.75;
        Value       : constant Float := 0.9;
        Chroma      : constant Float := Saturation * Value;
        H_Inc       : constant Float := 360.0 / Float (Num_Colours);
        Value_Shift : constant Float := Value - Chroma;
        H_Index     : Float := 25.0;
        H_Bar       : Float;
        X           : Float;
        RGB_Init    : RGB_Array;
        RGB_Index   : Positive;
        R           : Float;
        G           : Float;
        B           : Float;
        RGB_Item    : Integer_Graph_Colours;
        theColours  : Integer_Colours_List;
    begin
        while H_Index < 385.0 loop
            H_Bar := H_Index / 60.0;
            RGB_Index := 1 + Integer (H_Bar);
            X := Chroma * (1.0 - abs (Modulo (H_Bar, 2.0) - 1.0));
            RGB_Init := ((Chroma, X, 0.0),
                         (X, Chroma, 0.0),
                         (0.0, Chroma, X),
                         (0.0, X, Chroma),
                         (X, 0.0, Chroma),
                         (Chroma, 0.0, X),
                         (Chroma, X, 0.0));
            R := RGB_Init (RGB_Index).R;
            G := RGB_Init (RGB_Index).G;
            B := RGB_Init (RGB_Index).B;

            RGB_Item := (Integer (255.0 * (R + Value_Shift)),
                         Integer (255.0 * (G + Value_Shift)),
                         Integer (255.0 * (B + Value_Shift)));

            theColours.Append (RGB_Item);

            H_Index := H_Index + H_Inc;
        end loop;

        return theColours;

    end Colour_Brew;

    --  -------------------------------------------------------------------------

    procedure Export (Exporter    : in out DOT_Tree_Exporter;
                      Output_File : File_Type) is
        use Ada.Containers;
        Criteria : constant Criterion.Classifier_Criteria_Type :=
                     Criterion.Gini_Criteria;
    begin
        if not Exporter.Feature_Names.Is_Empty then
            Assert (Integer (Exporter.Feature_Names.Length) =
                      Exporter.theTree.Num_Features, "Graphviz_Exporter.Export" &
                      " Exporter.Feature_Names length" &
                      Count_Type'Image (Exporter.Feature_Names.Length) &
                      " does not match the number of features" &
                      Integer'Image (Exporter.theTree.Num_Features));
        end if;

        Head (Exporter, Output_File);
        Recurse (Exporter, Criteria, Output_File);
        Tail (Exporter, Output_File);

    end Export;

    --  -------------------------------------------------------------

    procedure Export_Graphviz (Exporter           : in out DOT_Tree_Exporter;
                               theTree            : Tree.Tree_Class;
                               Output_File_Name   : Unbounded_String :=
                                 To_Unbounded_String ("tree.dot");
                               Max_Depth          : Natural := Integer'Last;
                               Feature_Names      : Feature_Names_List :=
                                 Unbounded_Package.Empty_Vector;
                               Class_Names        : Class_Names_List :=
                                 Unbounded_Package.Empty_Vector;
                               Label              : Unbounded_String :=
                                 To_Unbounded_String ("all");
                               Filled             : Boolean := False;
                               Leaves_Parallel    : Boolean := False;
                               Impurity           : Boolean := True;
                               Node_Ids           : Boolean := False;
                               Proportion         : Boolean := False;
                               Rotate             : Float := 0.0;
                               Rounded            : Boolean := False;
                               Special_Characters : Boolean := False;
                               Precision          : Positive := 3;
                               Font_Name          : Unbounded_String :=
                                 To_Unbounded_String ("helvetica")) is
        Output_File : File_Type;
    begin
        Exporter.theTree := theTree;
        Exporter.Output_File_Name := Output_File_Name;
        Exporter.Max_Depth := Max_Depth;
        Exporter.Feature_Names := Feature_Names;
        Exporter.Class_Names := Class_Names;
        Exporter.Label := Label;
        Exporter.Filled := Filled;
        Exporter.Leaves_Parallel := Leaves_Parallel;
        Exporter.Impurity := Impurity;
        Exporter.Node_Ids := Node_Ids;
        Exporter.Proportion := Proportion;
        Exporter.Rotate := Rotate;
        Exporter.Rounded := Rounded;
        Exporter.Special_Characters := Special_Characters;
        Exporter.Precision  := Precision;
        Exporter.Font_Name := Font_Name;
        Exporter.Initialized := True;

        Create (Output_File, Out_File, To_String (Exporter.Output_File_Name));
        Export (Exporter, Output_File);
        Close (Output_File);

    end Export_Graphviz;

    --  -------------------------------------------------------------------------
    --  L225 Get_Colour finds the appropriate color & intensity for a node
    function Get_Colour (Exporter : in out DOT_Tree_Exporter;
                         Value    : Weights.Weight_List) return String is
        use Ada.Characters.Handling;
        use Ada.Integer_Text_IO;
        use Classifier_Types;
--          Routine_Name  : constant String :=
--                            "Graphviz_Exporter.Get_Colour ";
        Colour_Index  : constant Positive :=
                          Classifier_Utilities.Arg_Max (Value);
        Colour        : Integer_Graph_Colours;
        Sorted_Values : Float_List := Value;
        Alpha         : Float;
        Alpha_1       : Float;
        Pos           : Integer;

        function Set_Colour (Colour : Natural) return String is
            use Ada.Strings.Fixed;
            Hex_Colour    : String (1 .. 8);
            Dec_Colour    : Natural;
            Hex_Colour_UB : Unbounded_String;
        begin
            Dec_Colour := Integer (Float'Rounding (Alpha * Float (Colour) +
                                     Alpha_1));
            Put (Hex_Colour, Dec_Colour, Base => 16);

            Pos := Index (Hex_Colour, "#");
            Delete (Hex_Colour, 1, Pos); -- remove 16#
            Pos := Index (Hex_Colour, "#");
            Delete (Hex_Colour, Pos, Pos);
            Hex_Colour_UB := To_Unbounded_String
              (To_Lower (Trim (Hex_Colour, Ada.Strings.Right)));

            if Length (Hex_Colour_UB) = 0 then
                Hex_Colour_UB := To_Unbounded_String ("00");
            elsif Length (Hex_Colour_UB) = 1 then
                Hex_Colour_UB := To_Unbounded_String ("0") & Hex_Colour_UB;
            end if;

            return To_String (Hex_Colour_UB);

        end Set_Colour;

    begin
        if Exporter.Bounds.Is_Empty then
            --  L228 Classification Tree
            Colour := Exporter.Colours.Element (Colour_Index);
            Float_Sorting.Sort (Sorted_Values);
            Float_Package.Reverse_Elements  (Sorted_Values);
            if Integer (Sorted_Values.Length) = 1 then
                Alpha := 0.0;
            else
                Alpha :=
                  (Sorted_Values.Element (1) - Sorted_Values.Element (2)) /
                    (1.0 - Sorted_Values.Element (2));
            end if;

            Alpha_1 := 255.0 * (1.0 - Alpha);
        else
            --  Regression or multi-output
            null;
        end if;

        return "#" & Set_Colour (Colour.R) & Set_Colour (Colour.G) &
          Set_Colour (Colour.B);

    end Get_Colour;

    --  -------------------------------------------------------------------------
    --  L248 Get_Fill_Colour fetches the appropriate color for a node
    function Get_Fill_Colour (Exporter  : in out DOT_Tree_Exporter;
                              Node_Curs : Tree.Tree_Cursor;
                              Node_ID   : Positive) return String is
        use Weights;
        use Tree.Nodes_Package;
--          Routine_Name  : constant String :=
--                            "Graphviz_Exporter.Get_Fill_Colour ";
        Weighted_Samples : constant Float
          := Float (Element (Node_Curs).Weighted_Num_Node_Samples);
        Output_Values    : Weight_Lists_2D;
        Class_Values     : Weight_List;
        Class            : Float;
    begin
        if Exporter.Colours.Is_Empty then
            --  L251
            Exporter.Colours := Colour_Brew
              (Integer (Exporter.theTree.Classes.Element (1).Length));

            --  L253
            if Integer (Exporter.theTree.Num_Outputs) /= 1 then
                null;
            elsif Exporter.theTree.Num_Classes.Element (1) = 1 and
              Integer (Classifier_Utilities.Unique_Weights
                       (Exporter.theTree.Values).Length) > 1 then
                null;
                --                 Node_Value := Exporter.theTree.Values
            end if;
        end if;

        --  L259
        if Integer (Exporter.theTree.Num_Outputs) = 1  then
            Output_Values := Exporter.theTree.Values.Element (Node_ID);
            for index in Output_Values.First_Index ..
              Output_Values.Last_Index loop
                Class_Values.Append (Output_Values.Element (index));

                for class_index in Class_Values.First_Index ..
                  Class_Values.Last_Index loop
                    Class := Class_Values.Element (class_index);
                    Class_Values.Replace_Element
                      (class_index, Class / Weighted_Samples);
                end loop;
            end loop;
        end if;

        return Get_Colour (Exporter, Class_Values);

    end Get_Fill_Colour;

    --  -------------------------------------------------------------------------

    procedure Head (Exporter    : DOT_Tree_Exporter;
                    Output_File : File_Type) is
        Rounded_Filled : Unbounded_String := To_Unbounded_String ("");
        Edge_Line      : Unbounded_String := To_Unbounded_String ("");
    begin
        Put_Line (Output_File, "digraph Tree {");
        --  L479 Specify node aesthetics
        Put (Output_File, "node [shape = box");
        if Exporter.Filled and Exporter.Rounded then
            Rounded_Filled := Rounded_Filled & "filled, rounded";
        elsif Exporter.Filled then
            Rounded_Filled := Rounded_Filled & "filled";
        elsif Exporter.Rounded then
            Rounded_Filled := Rounded_Filled & "rounded";
        end if;

        --  L485
        if Exporter.Filled or Exporter.Rounded then
            Put (Output_File, ", style = """ & To_String (Rounded_Filled)
                 & """");
            Put (Output_File, ", color = ""black""");
        end if;

        Put (Output_File, ", fontname = """ & To_String (Exporter.Font_Name));
        Put_Line (Output_File, """];");

        --  L494 Specify graph & edge aesthetics
        if Exporter.Leaves_Parallel then
            Put (Output_File, "graph [ranksep = equally, splines = polyline] ");
        end if;

        Edge_Line := To_Unbounded_String
          ("edge [fontname = """ & To_String (Exporter.Font_Name) & """];");
        if Exporter.Rotate /= 0.0 then
            Put (Output_File, To_String (Edge_Line));
            Put_Line (Output_File, "rankdir = LR;");
        else
            Put_Line (Output_File, To_String (Edge_Line));
        end if;

    end Head;

    --  -------------------------------------------------------------------------

    procedure Recurse (Exporter    : in out DOT_Tree_Exporter;
                       Criteria    : Criterion.Classifier_Criteria_Type;
                       Output_File : File_Type; Depth : Natural := 0) is
        use Ada.Containers;
        use Tree.Nodes_Package;

        procedure Do_Node (Node_Curs : Tree.Tree_Cursor) is
            use Export_Types.Export_Maps;
--              Routine_Name : constant String :=
--                               "Graphviz_Exporter.Recurse.Do_Node ";
            Node_ID      : constant Positive := Element (Node_Curs).Node_ID;
            Node_ID_S    : constant String := Integer'Image (Node_ID);
            Node_ID_UB   : constant Unbounded_String :=
                             To_Unbounded_String (Node_ID_S);
            Depth_S      : constant Unbounded_String
              := To_Unbounded_String (Integer'Image (Depth));
            Base_Angles  : constant array (1 .. 2) of Float := (45.0, -45.0);
            Node_Parent  : Tree.Tree_Cursor;
            Parent_ID    : Positive;
            Left_Child   : Tree.Tree_Cursor;
            Angles       : array (1 .. 2) of Integer;
        begin
--              Put_Line (Routine_Name & "Node ID" & Node_ID_S);
            if Node_ID > 1 then
                Node_Parent := Parent (Node_Curs);
                Parent_ID := Element (Node_Parent).Node_ID;
            end if;

            --  L510
            if Depth <= Exporter.Max_Depth then
                if not Element (Node_Curs).Leaf_Node then
                    Left_Child := First_Child (Node_Curs);
                    --  L512 Collect ranks for for 'leaf' option in plot_options
                    if Element (Left_Child).Leaf_Node then
                        --  Add to Ranks map
                        Include (Exporter.Ranks, To_Unbounded_String ("leaves"),
                                 Node_ID_UB);
                    elsif not Exporter.Ranks.Contains (Depth_S) then
                        Replace (Exporter.Ranks, Depth_S, Node_ID_UB);
                    else
                        Include (Exporter.Ranks, Depth_S, Node_ID_UB);
                    end if;
                end if; --  not leaf node

                --  L520
                Put (Output_File, Node_ID_S & " [label = """ &
                       Node_Strings.Node_To_String
                       (Exporter, Node_Curs, Criteria));

                --  L524
                if Exporter.Filled then
                    Put (Output_File, ", fillcolor = """ &
                           Get_Fill_Colour (Exporter, Node_Curs, Node_ID));
                end if;
                Put_Line (Output_File, """];");

                --  L530
                if Node_ID > 1 then
                    --  Add edge to parent
                    Put (Output_File,
                         Integer'Image (Parent_ID) & " -> " & Node_ID_S);
                    if Parent_ID = 1 then
                        --  L534 Draw True/False labels if parent is
                        --  "root" (top) node
                        for index in Angles'First .. Angles'Last loop
                            Angles (index) := - Integer ((2.0 * Exporter.Rotate - 1.0) *
                                                           Base_Angles (index));
                        end loop;
                        Put (Output_File,
                             " [labeldistance = 2.5, labelangle = ");

                        --  L537
                        if Node_ID = 2 then
                            Put (Output_File, Integer'Image (Angles (1)) &
                                   ", headlabel = ""True""]");
                        else
                            Put (Output_File, Integer'Image (Angles (2)) &
                                   ", headlabel = ""False""]");
                        end if;
                    end if;
                    --  L541
                    Put_Line (Output_File, ";");
                end if;
                --  L543 recurse

            else  --  L560 Depth > Exporter.Max_Depth
                Include (Exporter.Ranks, To_Unbounded_String ("leaves"),
                         Node_ID_UB);
                Put (Output_File,  Node_ID_S & " [label = ""(...)""");
                if Exporter.Filled then
                    --  Colour cropped nodes grey
                    Put (Output_File, ", fillcolor = ""#C0C0C0""");
                end if;
                Put_Line (Output_File, "];");

                --  L568
                if Node_ID > 1 then
                    --  Add edge to parent
                    Put_Line (Output_File,
                              Integer'Image (Element (Node_Parent).Node_ID)
                              & " -> " & Node_ID_S);
                end if;
            end if;

        end Do_Node;

    begin
        Assert (Exporter.theTree.Nodes.Node_Count > 1,
                "Graphviz_Exporter.Recurse, empty tree.");
        Exporter.theTree.Nodes.Iterate (Do_Node'Access);

    end Recurse;

    --  -------------------------------------------------------------------------

    procedure Tail (Exporter : DOT_Tree_Exporter; Output_File : File_Type) is
        use Export_Maps;
        procedure Write_Rank (Curs : Cursor) is
        begin
            Put_Line (Output_File,
                      "{Rank=same ; " & To_String (Element (Curs)) & ";");
            Put_Line (Output_File, "};");
        end Write_Rank;

    begin
        if Exporter.Leaves_Parallel then
            Exporter.Ranks.Iterate (Write_Rank'Access);
        end if;

        Put_Line (Output_File, "}");

    end Tail;

    --  -------------------------------------------------------------

end Graphviz_Exporter;
