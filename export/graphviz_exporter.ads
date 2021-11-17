
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types; use ML_Types;
with Tree;

package Graphviz_Exporter is

   type DOT_Tree_Exporter is private;

   procedure Dot_To_Dot (Input_File_Name : String);
   procedure Export_Graphviz (Exporter : DOT_Tree_Exporter);
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
                     To_Unbounded_String ("helvetica"));

private

   type DOT_Tree_Exporter is record
      theTree            : Tree.Tree_Nodes;
      Output_File_Name   : Unbounded_String := To_Unbounded_String ("tree.dot");
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
                             To_Unbounded_String ("helvetica");
      Initialized        : Boolean := False;
   end record;

end Graphviz_Exporter;
