
with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with NL_Types; use NL_Types;
with Printing;

with Openml_Ada; use Openml_Ada;

package body OML_File_Tests is

   --  -------------------------------------------------------------------------

   procedure Test_Fetch_OML is
      use Ada.Containers;
      Routine_Name   : constant String := "Test_Fetch_OML ";
      File_Name      : constant String := "../mnist_784.arff";
      Save_File      : constant String := "mnist_784.oml";
      As_Frame       : As_Frame_State := As_Frame_False;
      Target_Columns : String_List;
      Bunch          : Bunch_Data;
   begin
      Put_Line (Routine_Name);
      Target_Columns.Append (To_Unbounded_String ("class"));
      Fetch_Openml (Dataset_File_Name => File_Name,
                    Save_File_Name    => Save_File,
                    Target_Columns    => Target_Columns,
                    Bunch             => Bunch,
                    As_Frame          => As_Frame);

      Printing.Print_Strings (Routine_Name & "Target_Columns", Target_Columns);
      Put_Line (Routine_Name & "X length: " &
                  Count_Type'Image (Bunch.Data.Length));
      Put_Line (Routine_Name & "Y length: " &
                  Count_Type'Image (Bunch.Target.Length));
      Printing.Print_Integer_List (Routine_Name & "Y", Bunch.Target, 1, 20);

      if Bunch.Data.Length = Bunch.Target.Length then
         Put_Line (Routine_Name & "completed");
      else
         Put_Line (Routine_Name & "failed, Y length should equal X length.");
      end if;
      New_Line;

      pragma Unreferenced (As_Frame);

   end Test_Fetch_OML;

   --  -------------------------------------------------------------------------

end OML_File_Tests;
