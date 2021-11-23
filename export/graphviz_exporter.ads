
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types; use ML_Types;
with Export_Types;
with Tree;

package Graphviz_Exporter is

    type DOT_Tree_Exporter is record
        theTree            : Tree.Tree_Class;
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
                               To_Unbounded_String ("helvetica");
        Ranks              : Export_Types.Export_Map;
        Colours            : Export_Types.Integer_Colours_List;
        Bounds             : Export_Types.Bounds_List;
        Initialized        : Boolean := False;
    end record;

    Graphviz_Error : Exception;

    procedure Export_Graphviz (Exporter : in out DOT_Tree_Exporter;
                               theTree            : Tree.Tree_Class;
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
                                 To_Unbounded_String ("helvetica"));

end Graphviz_Exporter;
