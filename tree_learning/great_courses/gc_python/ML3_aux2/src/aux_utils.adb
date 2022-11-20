
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Aux_Utils is

   --  To return a list to the scripting language:
   procedure Handler (Data : in out Callback_Data'Class; Cmd : String) is
      aWord : constant String := "value";
      --  A List_Instance represents a list passed as parameter.
      --  Set_Nth_Arg will append to the list if the given index is outside of the
      --  current range of the list.
      --  Get_Script returns the scripting language that created Data
      List  : List_Instance := New_List (Get_Script (Data));
   begin
      Set_Nth_Arg (List, 1, Cmd);
      Set_Nth_Arg (List, 2, 12);
      Set_Nth_Arg (List, 2, 1.2);
      Set_Nth_Arg (List, 3, aWord);

      Set_Return_Value (Data, List);

   end Handler;

   --  -------------------------------------------------------------------------

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
