
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Neural_Utilities;

package body Support_8A is

   --  -------------------------------------------------------------------------

   function Load_Data (File_Name : String) return Data_Record is
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use ML_Types;
      use String_Package;
      --        File_ID  : File_Type;
      --        Data  : Real_Float_Matrix (1 .. Num_Samples, 1 .. 1);
      Raw_Data    : Unbounded_List;
      Words       : String_List;
      aWord       : Unbounded_String;
      Word_Cursor : Cursor;
      Data        : Data_Record;
   begin
      Raw_Data := Neural_Utilities.Load_CSV_Data (File_Name);
      for index in Raw_Data.First_Index .. Raw_Data.Last_Index loop
         Words :=
           Neural_Utilities.Split_String (To_String (Raw_Data (index)), ",");
         Word_Cursor := Words.First;
         while Has_Element (Word_Cursor) loop
            aWord := Element (Word_Cursor);
            Trim (aWord, Both);
            Words.Replace_Element (Word_Cursor, aWord);
            Next (Word_Cursor);
         end loop;

         if Words.First_Element = "pe-malicious" then
            Data.Labels.Append (1);
         else
            Data.Labels.Append (0);
         end if;

         Word_Cursor := Words.First;
         Next (Word_Cursor);
         while Has_Element (Word_Cursor) loop
            aWord := Element (Word_Cursor);
            Data.Features.Append
              (Float'Value (To_String (Element (Word_Cursor))));
            Next (Word_Cursor);
         end loop;

      end loop;

      return Data;

   end Load_Data;

   --  -------------------------------------------------------------------------

   function Test_Score (Predictions : Real_Float_Vector;
                        Labels      : Integer_Array) return Natural is
      Routine_Name : constant String := "Support_8A.Test_Score ";
      Correct      : Natural := 0;
      Incorrect    : Natural := 0;
   begin
      for index in Predictions'Range loop
         if Integer (Predictions (index)) = Labels (index) then
            Correct := Correct + 1;
         else
            Incorrect := Incorrect + 1;
            if Incorrect < 10 then
               Put_Line (Routine_Name & "incorrect prediction:" &
                           Float'Image (Predictions (index))  &  " for " &
                           Integer'Image (Labels (index)));
            end if;
         end if;
      end loop;

      Put_Line (Routine_Name & "correct predictions:" &
                  Integer'Image (Correct));
      Put_Line (Routine_Name & "incorrect predictions:" &
                  Integer'Image (Incorrect));

      return Correct;

   end Test_Score;

   --  -------------------------------------------------------------------------

end Support_8A;
