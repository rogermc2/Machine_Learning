
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

   procedure Export (Input_File_Name : String) is
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
   end Export;

   --  -------------------------------------------------------------------------

   procedure Export_Graphviz (theTree            : Tree.Tree_Nodes;
                              Output_File_Name   : Unbounded_String :=
                                To_Unbounded_String ("tree.dot");
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
                                To_Unbounded_String ("helvetica")) is

   begin
      null;
   end Export_Graphviz;

   --  -------------------------------------------------------------------------

end Graphviz_Exporter;
