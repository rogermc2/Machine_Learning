--  Based on scikit-learn/sklearn/tree _export.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities;
with Config;
with Criterion;
with Dot_Tables;
with Export_Types;
with Node_Strings;
with State_Machine;
with Export_Types; use Export_Types;
with Export_Utilities;
with Weights;

package body Graphviz_Exporter is

   type RGB_Array is array (1 .. 7) of Graph_Colours;

   procedure Head (Exporter    : DOT_Tree_Exporter;
                   Output_File : File_Type);
   procedure Recurse (Exporter    : in out DOT_Tree_Exporter;
                      Criteria    : Criterion.Classifier_Criteria_Type;
                      Output_File : File_Type; Depth : Natural := 0);
   procedure Tail (Exporter    : DOT_Tree_Exporter; Output_File : File_Type);

   --  -------------------------------------------------------------------------

   function Colour_Brew (Num_Colours : Positive) return Colours_List is
      Saturation  : constant Float := 0.75;
      Value       : constant Float := 0.9;
      Chroma      : constant Float := Saturation * Value;
      Value_Shift : constant Float := Value - Chroma;
      H_Index     : Float := 25.0;
      H_Bar       : Float;
      X           : Float;
      RGB_Init    : RGB_Array;
      R           : Float;
      G           : Float;
      B           : Float;
      RGB         : RGB_Array;
      theColours  : Colours_List;
   begin
      while H_Index < 385.0 loop
         H_Bar := H_Index / 60.0;
         X := Chroma * (1.0 - abs (Float'Remainder (H_Bar, 2.0)) - 1.0);
         RGB_Init := ((Chroma, X, 0.0),
                      (X, Chroma, 0.0),
                      (0.0, Chroma, X),
                      (0.0, X, Chroma),
                      (X, 0.0, Chroma),
                      (Chroma, 0.0, X),
                      (Chroma, X, 0.0));
         R := RGB_Init (Integer (H_Bar)).R;
         G := RGB_Init (Integer (H_Bar)).G;
         B := RGB_Init (Integer (H_Bar)).B;

         for index in RGB'First .. RGB'Last loop
            RGB (index) := (255.0 * (R + Value_Shift),
                            255.0 * (G + Value_Shift),
                            255.0 * (B + Value_Shift));
            theColours.Append (RGB (index));
         end loop;

         H_Index := H_Index + 360.0 / Float (Num_Colours);
      end loop;

      return theColours;

   end Colour_Brew;

   --  -------------------------------------------------------------------------

   procedure Dot_To_Dot (Input_File_Name : String) is
      Table            : Dot_Tables.Table_Data;
      Input            : File_Type;
      Output           : File_Type;
      --  The output file name is the input file name with "-1" added.
      Output_File_Name : constant String := Ada.Strings.Fixed.Insert
        (Input_File_Name, Ada.Strings.Fixed.Index (Input_File_Name, "."), "-1");
      S                : Export_Types.Lines.Bounded_String;
      N                : Config.Name;
      Finish           : Natural := 0;
      Continue         : Boolean := True;
   begin
      Open (Input, In_File, Input_File_Name);
      Create (Output, Out_File, Output_File_Name);

      while not End_Of_File (Input) loop
         S := Export_Types.Lines_IO.Get_Line (Input);
         Continue := True;
         while Continue loop
            Export_Utilities.Get_Name (S, Finish, N);
            Continue := Finish /= 0;
            if Continue then
               State_Machine.Parse_Line (Table, N);
            end if;
         end loop;
      end loop;

      Close (Input);

      Dot_Tables.Sort (Table);
      Dot_Tables.Put (Table, Output);
      Close (Output);

   exception
      when E : Syntax_Error =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
                                 " expected");
         Export_Types.Lines_IO.Put_Line (S);
   end Dot_To_Dot;

   --  -------------------------------------------------------------------------

   procedure Export (Exporter    : in out DOT_Tree_Exporter;
                     Output_File : File_Type) is
      use Ada.Containers;
      Criteria : constant Criterion.Classifier_Criteria_Type :=
                   Criterion.Gini_Criteria;
   begin
      if not Exporter.Feature_Names.Is_Empty then
         Assert (Exporter.Feature_Names.Length =
                   Exporter.theTree.Features.Length, "Exporter.Feature_Names"
                 & " length does not match the number of features.");
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
                              Max_Depth          : Positive := Integer'Last;
                              Feature_Names      : Feature_Names_List :=
                                Unbounded_Package.Empty_Vector;
                              Class_Names        : Class_Names_Array;
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
      Exporter.Class_Names.Clear;
      for index in Class_Names'First .. Class_Names'Last loop
         Exporter.Class_Names.Append (Class_Names (index));
      end loop;
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
      use Ada.Integer_Text_IO;
      use Classifier_Types;
      Colour_Index  : constant Positive := Classifier_Utilities.Arg_Max (Value);
      Colour        : Integer_Graph_Colours;
      Sorted_Values : Float_List := Value;
      Alpha         : Float;

      function Set_Colour (Colour : in out Integer) return String is
         Hex_Colour       : String (1 .. 2);
      begin
         Colour := Integer (Float'Rounding (Alpha * Float (Colour) +
                              255.0 * (1.0 - Alpha)));
         Put (Hex_Colour, Colour, Base => 16);
         return Hex_Colour;
      end Set_Colour;

   begin
      if Exporter.Bounds.Is_Empty then
         --  Classification Tree
         Colour := Exporter.Colours.Element (Colour_Index);
         Float_Sorting.Sort (Sorted_Values);
         if Integer (Sorted_Values.Length) = 1 then
            Alpha := 0.0;
         else
            Alpha :=
              (Sorted_Values.Element (1) - Sorted_Values.Element (2)) /
                (1.0 - Sorted_Values.Element (2));
         end if;
      else
         --  Regression or multi-output
         null;
      end if;

      return "#" & Set_Colour (Colour.R) & Set_Colour (Colour.G) &
        Set_Colour (Colour.B);

   end Get_Colour;

   --  -------------------------------------------------------------------------
   --  L248 Get_Fill_Colour fetches the appropriate color for a node
   function Get_Fill_Colour (Exporter : in out DOT_Tree_Exporter;
                             Node_ID  : Positive) return String is
      use Weights;
      Node_Values_List : Weight_Lists_2D;
      Node_Value       : Weight_List;
      Colours          : Colours_List;
   begin
      if Exporter.Colours.Is_Empty then
         --  L251
         Colours := Colour_Brew (Integer (Exporter.theTree.Classes.Length));
         for index in Colours.First_Index .. Colours.Last_Index loop
            Exporter.Colours.Append ((Integer (Colours.Element (index).R),
                                     Integer (Colours.Element (index).G),
                                     Integer (Colours.Element (index).B)));
         end loop;

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
         Node_Values_List := Exporter.theTree.Values.Element (Node_ID);
         for index in Node_Values_List.First_Index ..
           Node_Values_List.Last_Index loop
            Node_Value.Append (Node_Values_List.Element (index));
         end loop;
      end if;

      return Get_Colour (Exporter, Node_Value);

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
      if Exporter.Filled then
         Rounded_Filled := Rounded_Filled & "filled";
      end if;

      if Exporter.Rounded then
         Rounded_Filled := Rounded_Filled & "rounded";
      end if;

      --  L485
      if Exporter.Filled or Exporter.Rounded then
         Put (Output_File, ", style = " & To_String (Rounded_Filled));
         Put (Output_File, ", color = ""black""");
      end if;

      Put (Output_File, ", fontname = " & To_String (Exporter.Font_Name));
      Put_Line (Output_File, "];");

      --  L494 Specify graph & edge aesthetics
      if Exporter.Leaves_Parallel then
         Put (Output_File, "graph [ranksep = equally, splines = polyline] ");
      end if;

      Edge_Line := To_Unbounded_String
        ("edge [fontname = " & To_String (Exporter.Font_Name) & "];");
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
         Routine_Name : constant String :=
                          "Graphviz_Exporter.Recurse.Do_Node ";
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
         Angles       : array (1 .. 2) of Float;
      begin
         Put_Line (Routine_Name & "Node ID" & Node_ID_S);
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
               Put (Output_File, ", fillcolor = " &
                      Get_Fill_Colour (Exporter, Node_ID));
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
                     Angles (index) := -(2.0 * Exporter.Rotate - 1.0) *
                       Base_Angles (index);
                  end loop;
                  Put (Output_File,
                       " [labeldistance = 2.5, labelangle = ");

                  --  L537
                  if Node_ID = 2 then
                     Put (Output_File, Float'Image (Angles (1)) &
                            ", headlabel = ""True""");
                  else
                     Put (Output_File, Float'Image (Angles (2)) &
                            ", headlabel = ""False""");
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
