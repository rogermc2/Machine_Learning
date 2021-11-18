
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Config;
with Dot_Tables;
with State_Machine;
with Export_Types; use Export_Types;
with Export_Utilities;

package body Graphviz_Exporter is

   type Rank is (Leaves_Rank, Colours_Rank);
   type Colour is (Bounds_Colour, RGB_Colour);

   procedure Head (Exporter    : DOT_Tree_Exporter;
                   Output_File : File_Type);
   procedure Recurse (Exporter : in out DOT_Tree_Exporter;
                      Output_File : File_Type;
                      Depth    : Natural := 0);

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
   begin
      Head (Exporter, Output_File);
      Recurse (Exporter, Output_File);
   end Export;

   --  -------------------------------------------------------------

   procedure Export_Graphviz (Exporter : in out DOT_Tree_Exporter) is
      Output_File : File_Type;
   begin
      Create (Output_File, Out_File, To_String (Exporter.Output_File_Name));
      Export (Exporter, Output_File);
      Close (Output_File);

   end Export_Graphviz;

   --  -------------------------------------------------------------------------

   procedure Head (Exporter    : DOT_Tree_Exporter;
                   Output_File : File_Type) is
      Rounded_Filled : Unbounded_String := To_Unbounded_String ("");
      Edge_Line      : Unbounded_String := To_Unbounded_String ("");
   begin
      Put_Line (Output_File, "digraph Tree");
      --  Specify node aesthetics
      Put (Output_File, "node [shape=box");
      if Exporter.Filled then
         Rounded_Filled := Rounded_Filled & "filled";
      end if;
      if Exporter.Rounded then
         Rounded_Filled := Rounded_Filled & "rounded";
      end if;
      if Exporter.Filled or Exporter.Rounded then
         Put (Output_File, ", style=" & To_String (Rounded_Filled));
         Put (Output_File, ", color=""black""");
      end if;
      Put (Output_File, ", fontname=" & To_String (Exporter.Font_Name));
      Put_Line (Output_File, "] ;\n");
      --  Specify graph & edge aesthetics
      if Exporter.Leaves_Parallel then
         Put_Line (Output_File, "graph [ranksep=equally, splines=polyline] ");
      end if;
      Edge_Line := To_Unbounded_String ("edge [fontname=""%s""] ;\n" &
                                          To_String (Exporter.Font_Name));
      if Exporter.Rotate then
         Put (Output_File, To_String (Edge_Line));
         Put_Line (Output_File, "rankdir=LR ;\n");
      else
         Put_Line (Output_File, To_String (Edge_Line));
      end if;

   end Head;

   --  -------------------------------------------------------------

   procedure Init (Exporter           : in out DOT_Tree_Exporter;
                   theTree            : Tree.Tree_Nodes;
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
                   Rotate             : Boolean := False;
                   Rounded            : Boolean := False;
                   Special_Characters : Boolean := False;
                   Precision          : Positive := 3;
                   Font_Name          : Unbounded_String :=
                     To_Unbounded_String ("helvetica")) is

   begin
      Exporter.theTree := theTree;
      Exporter.Output_File_Name := Output_File_Name;
      Exporter.Max_Depth := Max_Depth;
      Exporter.Feature_Names := Feature_Names;
      Exporter.Class_Names := Class_Names;
      Exporter.Label := Label;
      Exporter.Filled := Filled;
      Exporter.Leaves_Parallel:= Leaves_Parallel;
      Exporter.Impurity := Impurity;
      Exporter.Node_Ids := Node_Ids;
      Exporter.Proportion := Proportion;
      Exporter.Rotate := Rotate;
      Exporter.Rounded := Rounded;
      Exporter.Special_Characters := Special_Characters;
      Exporter.Precision  := Precision;
      Exporter.Font_Name := Font_Name;

      Exporter.Initialized := True;

   end Init;

   --  -------------------------------------------------------------------------

   procedure Recurse (Exporter : in out DOT_Tree_Exporter;
                      Output_File : File_Type;
                      Depth    : Natural := 0) is
      use Tree.Nodes_Package;

      procedure Do_Node (Curs : Cursor) is
         Left_Child  : Cursor;
         Right_Child : Cursor;
         Node_ID     : Positive;
         Pos         : Natural;
      begin
         if Curs /= Exporter.theTree.Root and then
           not Element (Curs).Leaf_Node and then
           Depth <= Exporter.Max_Depth then
            Node_ID := Element (Curs).Node_ID;
            Left_Child := First_Child (Curs);
            Right_Child := Last_Child (Curs);
            if Element (Left_Child).Leaf_Node then
               Exporter.Ranks := To_Unbounded_String
                 ("leaves" & Integer'Image (Node_ID));
            elsif Index (Exporter.Ranks, Integer'Image (Depth)) > 0 then
               null;
            else Exporter.Ranks := To_Unbounded_String
                 (Integer'Image (Depth) & Integer'Image (Node_ID));
            end if;

            Put_Line (Output_File, Integer'Image (Node_ID) & " [label=");
         end if;
      end Do_Node;

   begin
      Exporter.theTree.Iterate (Do_Node'Access);

   end Recurse;

   --  -------------------------------------------------------------------------

end Graphviz_Exporter;
