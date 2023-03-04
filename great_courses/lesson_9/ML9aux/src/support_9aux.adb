
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;
with Neural_Loader;

package body Support_9AUX is

   --  -------------------------------------------------------------------------

   function Get_Data (File_Name : String) return Data_Record is
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use ML_Types;
      use String_Package;
      Routine_Name : constant String := "Support_8A.Get_Data ";
      Raw_Data     : Unbounded_List;
      Row_Words    : String_List;
      aWord        : Unbounded_String;
      Word_Cursor  : Cursor;
      Items        : Real_Float_List;
      Features     : Real_Float_List_2D;
      Labels       : ML_Types.Integer_List;
   begin
      Raw_Data := Neural_Loader.Load_CSV_Data (File_Name);
      Put_Line (Routine_Name & File_Name & " loaded");

      for row in Raw_Data.First_Index .. Raw_Data.Last_Index loop
         Row_Words :=
           Neural_Loader.Split_String (To_String (Raw_Data (row)), ",");
         Word_Cursor := Row_Words.First;
         while Has_Element (Word_Cursor) loop
            aWord := Element (Word_Cursor);
            Trim (aWord, Both);
            Row_Words.Replace_Element (Word_Cursor, aWord);
            Next (Word_Cursor);
         end loop;

         if Row_Words.First_Element = "pe-malicious" then
            Labels.Append (1);
         else
            Labels.Append (0);
         end if;

         Word_Cursor := Row_Words.First;
         Next (Word_Cursor);
         Items.Clear;
         while Has_Element (Word_Cursor) loop
            aWord := Element (Word_Cursor);
            Items.Append
              (Float'Value (To_String (Element (Word_Cursor))));
            Next (Word_Cursor);
         end loop;

         Features.Append (Items);
      end loop;

      declare
         Data  : Data_Record (Integer (Features.Length),
                              Integer (Features (1).Length));
      begin
         Data.Features := To_Real_Float_Matrix (Features);
         Data.Labels := To_Integer_Array (Labels);
         return Data;
      end;

   end Get_Data;

   --  -------------------------------------------------------------------------

   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Array) return Float is
--        Routine_Name : constant String := "Support_9AUX.Error ";
      Incorrect    : Natural := 0;
   begin
      for index in Predictions'Range loop
         if Integer (Predictions (index)) /= Labels (index) then
            Incorrect := Incorrect + 1;
         end if;
      end loop;

      return Float (Incorrect) / Float (Labels'Length);

   end Error;

   --  -------------------------------------------------------------------------

end Support_9AUX;
