
with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Config;
with Dot_Tables;
with State_Machine;
with Export_Types; use Export_Types;
with Export_Utilities;
with Weights;

package body Graphviz_Exporter is

    procedure Head (Exporter    : DOT_Tree_Exporter;
                    Output_File : File_Type);
    procedure Recurse (Exporter    : in out DOT_Tree_Exporter;
                       Output_File : File_Type; Depth : Natural := 0);
    procedure Tail (Exporter    : DOT_Tree_Exporter; Output_File : File_Type);

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

    --     function Get_Fill_Colour (Exporter : DOT_Tree_Exporter;
    --                               Node_ID : Positive) return String is
    --     begin
    --        if not Exporter.Ranks.Contains (To_Unbounded_String ("rgb")) then
    --           null;
    --        end if;
    --        return "";
    --     end Get_Fill_Colour;

    --  -------------------------------------------------------------------------

    procedure Head (Exporter    : DOT_Tree_Exporter;
                    Output_File : File_Type) is
        Rounded_Filled : Unbounded_String := To_Unbounded_String ("");
        Edge_Line      : Unbounded_String := To_Unbounded_String ("");
    begin
        Put_Line (Output_File, "digraph Tree {");
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
        Put_Line (Output_File, "] ;");
        --  Specify graph & edge aesthetics
        if Exporter.Leaves_Parallel then
            Put (Output_File, "graph [ranksep=equally, splines=polyline] ");
        end if;
        Edge_Line := To_Unbounded_String
          ("edge [fontname=" & To_String (Exporter.Font_Name) & "];");
        if Exporter.Rotate /= 0.0 then
            Put (Output_File, To_String (Edge_Line));
            Put_Line (Output_File, "rankdir=LR;");
        else
            Put_Line (Output_File, To_String (Edge_Line));
        end if;

    end Head;

    --  -------------------------------------------------------------------------
    --  Node_To_String generates the node content string
    procedure Node_To_String
      (Exporter  : DOT_Tree_Exporter; Node_Curs : Tree.Tree_Cursor;
       Output_File : File_Type) is
        use Ada.Containers;
        use Tree.Nodes_Package;
--          Routine_Name   : constant String := "Graphviz_Exporter.Node_To_String";
        Node_ID        : constant Positive := Element (Node_Curs).Node_ID;
        Node_ID_S    : constant String := Integer'Image (Node_ID);
        Top_Node       : constant Tree.Tree_Cursor :=
                           First_Child (Exporter.theTree.Nodes.Root);
        Node_Data      : constant Tree.Tree_Node := Element (Node_Curs);
        Classes        : constant ML_Types.Value_Data_Lists_2D :=
                           Exporter.theTree.Classes;
        Show_Labels    : constant Boolean
          := Exporter.Label = To_Unbounded_String ("all") or
          (Exporter.Label = To_Unbounded_String ("root") and Node_ID = 1);
        Left_Child     : constant Tree.Tree_Cursor := First_Child (Node_Curs);
        Feature_Names  : constant Feature_Names_List := Exporter.Feature_Names;
        --  Value: num_outputs x num_classes
        Value          : Weights.Weight_Lists_2D :=
                           Exporter.theTree.Values.Element (Node_ID);
        Output_Data    : Weights.Weight_List;
        Class_Value    : Float;
        Arg_Max        : Positive;
        Class_Name     : Unbounded_String;
        Feature        : Unbounded_String;
        Value_First    : Boolean := True;
        Value_Text     : Unbounded_String := To_Unbounded_String ("");
        Pos            : Natural;
        Percent        : Float;
        Node_String    : Unbounded_String := To_Unbounded_String ("");
    begin
        Node_String := Node_ID_S & To_Unbounded_String (" [label=");
        if Exporter.Node_Ids then
            --  Write node ID
            if Show_Labels then
                Node_String := Node_String & "node ";
            end if;
            Node_String := Node_String & "#" & Node_ID_S;
            Put_Line (Output_File, To_String (Node_String));
            Node_String := To_Unbounded_String ("");
        end if;

        if not Element (Left_Child).Leaf_Node then
            --  Write decision criteria
            if not Exporter.Feature_Names.Is_Empty then
                Feature := Feature_Names.Element (Node_ID);
            else
                Feature := To_Unbounded_String ("X[") &
                  Feature_Names.Element (Node_ID) & "]";
            end if;

            Node_String := Node_String & Feature &  " => " &
              Classifier_Utilities.Float_Precision
              (Node_Data.Threshold, Exporter.Precision);
            Put_Line (Output_File, To_String (Node_String));
            Node_String := To_Unbounded_String ("");
        end if;

        if Exporter.Impurity then
            if Show_Labels then
                Node_String := Node_String & "impurity = ";
            end if;

            Node_String := Node_String & Classifier_Utilities.Float_Precision
              (Node_Data.Impurity, Exporter.Precision);
            Put_Line (Output_File, To_String (Node_String));
            Node_String := To_Unbounded_String ("");
        end if;

        --  Write node samples count
        if Show_Labels then
            Node_String := Node_String & "samples = ";
        end if;

        if Exporter.Proportion then
            Percent := 100.0 * Float (Node_Data.Num_Node_Samples) /
              Float (Element (Top_Node).Num_Node_Samples);
            Node_String := Node_String &
              Classifier_Utilities.Float_Precision (Percent, 1) & "%";
        else
            Node_String := Node_String &
              Integer'Image (Node_Data.Num_Node_Samples);
        end if;
        Put_Line (Output_File, To_String (Node_String));
        Node_String := To_Unbounded_String ("");

        --  Write node class distribution / regression value
        if Exporter.Proportion and Classes.Element (1).Length /= 1 then
            for output_index in Value.First_Index .. Value.Last_Index loop
                Output_Data := Value.Element (output_index);
                for class_index in Output_Data.First_Index ..
                  Output_Data.Last_Index loop
                    Class_Value := Output_Data.Element (class_index);
                    Output_Data.Replace_Element
                      (class_index, Class_Value /
                         Float (Node_Data.Num_Node_Samples));
                end loop;
                Value.Replace_Element (output_index, Output_Data);
            end loop;
        end if;

        if Show_Labels then
            Node_String := Node_String & "value = ";
        end if;

        if Exporter.Proportion then
            for output_index in Value.First_Index .. Value.Last_Index loop
                Output_Data := Value.Element (output_index);
                for class_index in Output_Data.First_Index ..
                  Output_Data.Last_Index loop
                    if Value_First then
                        Value_First := False;
                    else
                        Value_Text := Value_Text & ", ";
                    end if;
                    Value_Text := Value_Text &
                    (To_Unbounded_String (Classifier_Utilities.Float_Precision
                     (Output_Data.Element (class_index), Exporter.Precision)));
                end loop;
            end loop;
        end if;

        if Integer (Exporter.theTree.Num_Outputs) = 1 and then
          Exporter.theTree.Classes.Element (1).Element (1).Value_Kind =
          Integer_Type then
            Pos := Index (Value_Text, "[");
            while Pos /= 0 loop
                Delete (Value_Text, Pos, Pos);
                Pos := Index (Value_Text, "[");
            end loop;
            Pos := Index (Value_Text, "]");
            while Pos /= Length (Value_Text) loop
                Delete (Value_Text, Pos, Pos);
                Pos := Index (Value_Text, "]");
            end loop;
            Value_Text := Value_Text & "";
        end if;
        Put_Line (Output_File, To_String (Node_String & Value_Text));
        Node_String := To_Unbounded_String ("");

        --  Write node majority class
        if not Exporter.Class_Names.Is_Empty and then
          Integer (Exporter.theTree.Num_Outputs) = 1 and then
          Classes.Element (1).Element (1).Integer_Value /= 1 then
            if Show_Labels then
                Node_String := Node_String & "class = ";
            end if;

            Output_Data := Value.Element (Node_ID);
            Arg_Max := Weights.Max (Output_Data);

            if Exporter.Class_Names.Is_Empty then
                Class_Name := Exporter.Class_Names.Element (Arg_Max);
            else
                Class_Name := "y[" &
                  To_Unbounded_String (Integer'Image (Arg_Max)) & "]";
            end if;
            Put_Line (Output_File, To_String (Node_String & Class_Name));
        end if;

    end Node_To_String;

    --  -------------------------------------------------------------------------

    procedure Recurse (Exporter    : in out DOT_Tree_Exporter;
                       Output_File : File_Type; Depth : Natural := 0) is

        procedure Do_Node (Node_Curs : Tree.Tree_Cursor) is
            use Tree.Nodes_Package;
            use Export_Types.Export_Maps;
            --           Routine_Name : constant String := "Graphviz_Exporter.Recurse.Do_Node";
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
            if Node_Curs /= Exporter.theTree.Nodes.Root then
                if not Element (Node_Curs).Leaf_Node and then
                  Depth <= Exporter.Max_Depth then
                    Left_Child := First_Child (Node_Curs);
                    --                 Right_Child := Last_Child (Node_Curs);
                    --  Collect ranks for for 'leaf' option in plot_options
                    if Element (Left_Child).Leaf_Node then
                        Include (Exporter.Ranks, To_Unbounded_String ("leaves"),
                                 Node_ID_UB);
                        --              elsif not Exporter.Ranks.Contains (Depth_S) then
                        --                 Include (Exporter.Ranks, Depth_S, Node_ID);
                    else
                        Include (Exporter.Ranks, Depth_S, Node_ID_UB);
                    end if;

                    Node_To_String (Exporter, Node_Curs, Output_File);

                    if Exporter.Filled then
                        Put (Output_File, ", fillcolor=");
                    end if;
                    Put_Line (Output_File, "];");

                    if Node_Parent /= Exporter.theTree.Nodes.Root then
                        --  Add edge to parent
                        Put (Output_File,
                             Integer'Image (Element (Node_Parent).Node_ID) &
                               " -> " & Node_ID_S);
                    else
                        for index in Angles'First .. Angles'Last loop
                            Angles (index) := (1.0 - 2.0 * Exporter.Rotate) * Angles (index);
                        end loop;
                        Put (Output_File, " [labeldistance=2.5, labelangle=");
                        if Node_ID = 1 then
                            Put_Line (Output_File, Float'Image (Angles (1)) &
                                        ", headlabel=""True"";");
                        else
                            Put_Line (Output_File,  Float'Image (Angles (2)) &
                                        ", headlabel=""False"";");
                        end if;
                    end if;

                    if not Element (Left_Child).Leaf_Node then
                        null;
                    end if;
                else  --   Leaf_Node or Depth > Exporter.Max_Depth
                    Include (Exporter.Ranks, To_Unbounded_String ("leaves"),
                             Node_ID_UB);
                    Put (Output_File,  Node_ID_S & " [label=""(...)""");
                    if Exporter.Filled then
                        --  Colour cropped nodes grey
                        Put (Output_File, ", fillcolor=""#C0C0C0""");
                    end if;
                    Put_Line (Output_File, "];");

                    if Element (Node_Parent).Node_ID > 1 then
                        --  Add edge to parent
                        Put_Line (Output_File,
                                  Integer'Image (Element (Node_Parent).Node_ID)
                                  & " -> " & Node_ID_S);
                    end if;
                end if;
            end if;  --  Curs /= Exporter.theTree.Root

        end Do_Node;

    begin
        Exporter.theTree.Nodes.Iterate (Do_Node'Access);

    end Recurse;

    --  -------------------------------------------------------------------------

    procedure Tail (Exporter : DOT_Tree_Exporter; Output_File : File_Type) is
        use Export_Maps;
        procedure Write_Rank (Curs : Cursor) is
        begin
            Put (Output_File, "{Rank=same ; " & To_String (Element (Curs)) & "; ");

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
