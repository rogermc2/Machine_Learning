
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Aux_Utils is

   function Load_Data (File_Name : String)
                       return ML_Types.Unbounded_List is
      Routine_Name : constant String := "Aux_Utils.Load_Data ";
      Data_File    : File_Type;
      Data         : ML_Types.Unbounded_List;
   begin

      Open (Data_File, In_File, File_Name);

      while not End_Of_File (Data_File) loop
         Data.Append (To_Unbounded_String (Get_Line (Data_File)));
      end loop;

      Close (Data_File);

      return Data;

   exception
      when others =>
         Put_Line (Routine_Name & "failed.");
      return Data;

   end Load_Data;

   --  -------------------------------------------------------------------------

end Aux_Utils;
