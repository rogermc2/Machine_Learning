
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Config;
with Dot_Tables;
with State_Machine;
with Export_Types; use Export_Types;
with Export_Utilities;

package body Graphviz_Exporter is

    type DOT_Tree_Exporter is record
        theTree            : Tree.Tree_Nodes;
        Output_File_Name   : Unbounded_String := To_Unbounded_String ("tree.dot");
        Output_File        : File_Type;
        Max_Depth          : Positive := Integer'Last;
        Feature_Names      : Feature_Names_List :=
                               Unbounded_Package.Empty_Vector;
        Class_Names        : Class_Names_List :=
                               Unbounded_Package.Empty_Vector;
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
                               To_Unbounded_String ("helvetica");
    end record;

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

    procedure Export (Exporter : DOT_Tree_Exporter;
                      Output_File : File_Type;
                      theTree : Tree.Tree_Nodes) is
        use Unbounded_Package;
    begin
        if not Exporter.Feature_Names.Is_Empty then
            null;
        end if;
    end Export;

    --  -------------------------------------------------------------

    procedure Export_Graphviz (theTree            : Tree.Tree_Nodes;
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

        Exporter    : DOT_Tree_Exporter;
        Output_File : File_Type;
    begin
        Exporter.Output_File_Name := Output_File_Name;
        Exporter.Feature_Names := Feature_Names;
        Create (Output_File, Out_File, To_String (Output_File_Name));
        Export (Exporter, Output_File, theTree);
        Close (Output_File);

    end Export_Graphviz;

    --  -------------------------------------------------------------------------

    procedure Head (Exporter : DOT_Tree_Exporter;
                    Output_File : File_Type;
                    theTree : Tree.Tree_Nodes) is
        use Unbounded_Package;
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
            Put (Output_File, ", style=""%s"", color=""black""");
            Put (Output_File, ", " & To_String (Rounded_Filled));
       end if;
       Put (Output_File, ", fontname=""%s""" & To_String (Exporter.Font_Name));
       Put_Line (Output_File, "] ;\n");
       --  Specify graph & edge aesthetics
       if Exporter.Leaves_Parallel then
            Put_Line (Output_File, "graph [ranksep=equally, splines=polyline] ");
       end if;
       Edge_Line := To_Unbounded_String ("edge [fontname=""%s""] ;\n" &
                                           To_String (Exporter.Font_Name));
       if Exporter.Rotate then
            Put (To_String (Edge_Line));
            Put_Line (Output_File, "rankdir=LR ;\n");
       else
            Put_Line (To_String (Edge_Line));
       end if;

    end Head;

    --  -------------------------------------------------------------

end Graphviz_Exporter;
