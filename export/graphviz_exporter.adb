--  Based on scikit-learn/sklearn/tree _export.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Config;
with Criterion;
with Dot_Tables;
with Export_Types;
with Node_Strings;
with State_Machine;
with Export_Types; use Export_Types;
with Export_Utilities;

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
      Saturation  : Float := 0.75;
      Value       : Float := 0.9;
      Chroma      : Float := Saturation * Value;
      Value_Shift : Float := Value - Chroma;
      H_Index     : Float := 25.0;
      H_Bar       : Float;
      X           : Float;
      RGB_Init    : constant RGB_Array :=
                      ((Chroma, X, 0.0),
                       (X, Chroma, 0.0),
                       (0.0, Chroma, X),
                       (0.0, X, Chroma),
                       (X, 0.0, Chroma),
                       (Chroma, 0.0, X),
                       (Chroma, X, 0.0));
      R           : Float;
      G           : Float;
      B           : Float;
      RGB         : RGB_Array;
      theColours  : Colours_List;
   begin
      while H_Index < 385.0 loop
         H_Bar := H_Index / 60.0;
         X := Chroma * (1.0 - abs (Float'Remainder (H_Bar, 2.0)) - 1.0);
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

   function Get_Fill_Colour (Exporter : DOT_Tree_Exporter;
                             Node_ID  : Positive) return String is
      use Export_Types;
   begin
      if not Exporter.Ranks.Contains (To_Unbounded_String ("rgb")) then
         Exporter.Ranks.Include (To_Unbounded_String ("rgb"),
                                 Colour_Brew (Exporter.theTree.Classes.Element (1)));
      end if;
      return "";
   end Get_Fill_Colour;

   --  -------------------------------------------------------------------------

   procedure Head (Exporter    : DOT_Tree_Exporter;
                   Output_File : File_Type) is
      Rounded_Filled : Unbounded_String := To_Unbounded_String ("");
      Edge_Line      : Unbounded_String := To_Unbounded_String ("");
   begin
      Put_Line (Output_File, "digraph Tree {");
      --  Specify node aesthetics
      Put (Output_File, "node [shape = box");
      if Exporter.Filled then
         Rounded_Filled := Rounded_Filled & "filled";
      end if;

      if Exporter.Rounded then
         Rounded_Filled := Rounded_Filled & "rounded";
      end if;

      if Exporter.Filled or Exporter.Rounded then
         Put (Output_File, ", style = " & To_String (Rounded_Filled));
         Put (Output_File, ", color = ""black""");
      end if;

      Put (Output_File, ", fontname = " & To_String (Exporter.Font_Name));
      Put_Line (Output_File, "];");
      --  Specify graph & edge aesthetics
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
      use Tree.Nodes_Package;

      procedure Do_Node (Node_Curs : Tree.Tree_Cursor) is
         use Classifier_Utilities;
         use Export_Types.Export_Maps;
         Routine_Name : constant String :=
                          "Graphviz_Exporter.Recurse.Do_Node ";
         Node_Parent  : constant Tree.Tree_Cursor := Parent (Node_Curs);
         Node_ID      : constant Positive := Element (Node_Curs).Node_ID;
         Node_ID_S    : constant String := Integer'Image (Node_ID);
         Node_ID_UB   : constant Unbounded_String :=
                          To_Unbounded_String (Node_ID_S);
         Depth_S      : constant Unbounded_String
           := To_Unbounded_String (Integer'Image (Depth));
         Left_Child   : Tree.Tree_Cursor;
         Angles       : array (1 .. 2) of Float := (45.0, -45.0);
      begin
         Put_Line (Routine_Name & "Node ID" & Node_ID_S);
         if not Element (Node_Curs).Leaf_Node and then
         --  L510
           Depth <= Exporter.Max_Depth then
            Left_Child := First_Child (Node_Curs);
            --  L512 Collect ranks for for 'leaf' option in plot_options
            if Element (Left_Child).Leaf_Node then
               --  Add to Ranks map
               Include (Exporter.Ranks, To_Unbounded_String ("leaves"),
                        Node_ID_UB);
               --              elsif not Exporter.Ranks.Contains (Depth_S) then
               --                 Include (Exporter.Ranks, Depth_S, Node_ID);
            else
               Include (Exporter.Ranks, Depth_S, Node_ID_UB);
            end if;

            --  L520
            Put (Output_File, Node_ID_S & " [label = """ &
                   Node_Strings.Node_To_String (Exporter, Node_Curs, Criteria));

            --  L524
            if Exporter.Filled then
               Put (Output_File, ", fillcolor = " &
                      Get_Fill_Colour(Exporter, Node_ID));
            end if;
            --  L528
            Put_Line (Output_File, """];");

            if Node_Parent /= Exporter.theTree.Nodes.Root then
               --  L531  Add edge to parent
               Put (Output_File,
                    Integer'Image (Element (Node_Parent).Node_ID) &
                      " -> " & Node_ID_S);
            else
               for index in Angles'First .. Angles'Last loop
                  Angles (index) := (1.0 - 2.0 * Exporter.Rotate) *
                    Angles (index);
               end loop;
               Put (Output_File, " [labeldistance = 2.5, labelangle = ");
               if Node_ID = 1 then
                  Put_Line (Output_File,
                            Float_Precision (Angles (1), 3) &
                              ", headlabel = ""True""];");
               else
                  Put_Line (Output_File,  Float'Image (Angles (2)) &
                              ", headlabel = ""False""];");
               end if;
            end if;

            if not Element (Left_Child).Leaf_Node then
               null;
            end if;
         else  --   Leaf_Node or Depth > Exporter.Max_Depth
            Include (Exporter.Ranks, To_Unbounded_String ("leaves"),
                     Node_ID_UB);
            Put (Output_File,  Node_ID_S & " [label = ""(...)""");
            if Exporter.Filled then
               --  Colour cropped nodes grey
               Put (Output_File, ", fillcolor = ""#C0C0C0""");
            end if;
            Put_Line (Output_File, "];");

            if Element (Node_Parent).Node_ID > 1 then
               --  Add edge to parent
               Put_Line (Output_File,
                         Integer'Image (Element (Node_Parent).Node_ID)
                         & " -> " & Node_ID_S);
            end if;
         end if; --  not leaf node

      end Do_Node;

   begin
      Assert (Integer (Exporter.theTree.Nodes.Node_Count) > 1,
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
