
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;
with Neural_Utilities;

package body Support_8QS is

   function Get_Diff (Train : Real_Float_Matrix; Vec : Real_Float_Vector)
                      return Real_Float_Matrix;
   pragma Inline (Get_Diff);

   --  -------------------------------------------------------------------------

   function Get_Data (File_Name : String; Num_Samples : Natural := 0)
                      return Data_Record is
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use ML_Types;
      use String_Package;
      Routine_Name : constant String := "Support_8QS.Get_Data ";
      Last         : Positive;
      Raw_Data     : Unbounded_List;
      Row_Words    : String_List;
      aWord        : Unbounded_String;
      Word_Cursor  : Cursor;
      Items        : Real_Float_List;
      Features     : Real_Float_List_2D;
      Labels       : ML_Types.Integer_List;
   begin
      Raw_Data := Neural_Utilities.Load_CSV_Data (File_Name);
      Put_Line (Routine_Name & File_Name & " loaded");

      if Num_Samples = 0 or else Num_Samples > Integer (Raw_Data.Length) then
         Last := Integer (Raw_Data.Length);
      else  --  Num_Samples < Raw_Data.Length
         Last := Num_Samples;
      end if;

      for row in Raw_Data.First_Index .. Last loop
         Row_Words :=
           Neural_Utilities.Split_String (To_String (Raw_Data (row)), ",");
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

   function Get_Diff (Train : Real_Float_Matrix; Vec : Real_Float_Vector)
                      return Real_Float_Matrix is
      Diff : Real_Float_Matrix (Train'Range, Train'Range (2));
   begin
      for row in Train'Range loop
         for col in Train'Range (2) loop
            Diff (row, col) := (Train (row, col) - Vec (col)) ** 2;
            --              if Diff (row, col) = 0.0 then
            --                 Put_Line (" Get_Diffzero row col" & Integer'Image (row) &
            --                             ", " & Integer'Image (col));
            --              end if;
         end loop;
      end loop;

      return Diff;

   end Get_Diff;

   --  -------------------------------------------------------------------------
   --  Get_Mins finds the distances between each test data instance and its
   --  nearest neighbor in the training set.
   function Get_Mins (Train, Test : Real_Float_Matrix) return Real_Float_List is
      use Real_Float_Arrays;
--        Routine_Name   : constant String := "Support_8QS.Get_Mins ";
      Vec            : Real_Float_Vector (Test'Range (2));
      Diff           : Real_Float_Matrix (Train'Range, Train'Range (2));
      Total          : Real_Float_Vector (Train'Range);
      Dists          : Real_Vector_List;
      Min_Dists      : Real_Float_List;
   begin
      for row in Test'Range loop
         Vec := Get_Row (Test, row);

         Diff := Get_Diff (Train, Vec);
         Total := Sum (Diff);
         Dists.Append (Total);
         Min_Dists.Append (Min (Total));
      end loop;

      return Min_Dists;

   end Get_Mins;

   --  -------------------------------------------------------------------------

   function Test_Score (Predictions : Real_Float_Vector;
                        Labels      : Integer_Array) return Natural is
      --        Routine_Name : constant String := "Support_8QS.Test_Score ";
      Correct      : Natural := 0;
   begin
      for index in Predictions'Range loop
         if Integer (Predictions (index)) = Labels (index) then
            Correct := Correct + 1;
         end if;
      end loop;

      return Correct;

   end Test_Score;

   --  -------------------------------------------------------------------------

end Support_8QS;
