
with Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;
with Neural_Loader;

package body Support_10A is

   --  -------------------------------------------------------------------------

   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Matrix) return Float is
      --        Routine_Name : constant String := "Support_10A.Test_Score ";
      Incorrect    : Natural := 0;
   begin
      for index in Predictions'Range loop
         if Integer (Predictions (index)) /= Labels (index, 1) then
            Incorrect := Incorrect + 1;
         end if;
      end loop;

      return Float (Incorrect) / Float (Labels'Length);

   end Error;

   --  -------------------------------------------------------------------------

   function Get_Data (File_Name : String) return Data_Record is
      use Ada.Strings;
      use ML_Types;
      use String_Package;
      Routine_Name  : constant String := "Support_8A.Get_Data ";
      Raw_Data      : Unbounded_List;
      Row_Words     : String_List;
      aWord         : Unbounded_String;
      Word_Cursor   : Cursor;
   begin
      Raw_Data := Neural_Loader.Load_CSV_Data (File_Name);
      Put_Line (Routine_Name & File_Name & " loaded");

      declare
         Data_Length : constant Positive := Positive (Raw_Data.Length) - 1;
         Data        : Data_Record (Data_Length);
         Embarked    : String (1 .. 1);
      begin
         for row in Raw_Data.First_Index + 1 .. Raw_Data.Last_Index loop
            Row_Words :=
              Neural_Loader.Split_String (To_String (Raw_Data (row)), ",");
            Word_Cursor := Row_Words.First;
            while Has_Element (Word_Cursor) loop
               aWord := Element (Word_Cursor);
               Trim (aWord, Both);
               Row_Words.Replace_Element (Word_Cursor, aWord);
               Next (Word_Cursor);
            end loop;

            Word_Cursor := Row_Words.First;  --  PassengerId
            Data.Labels (row - 1) :=
              Integer'Value (To_String (Element (Word_Cursor)));
            Next (Word_Cursor);              --  Survived
            Next (Word_Cursor);              --  Pclass
            Data.Features (row - 1).P_Class :=
              Integer'Value (To_String (Element (Word_Cursor)));
            Next (Word_Cursor);              --  First Name
            Next (Word_Cursor);              --  Surname Name
            Next (Word_Cursor);              --  Sex
            if To_String (Element (Word_Cursor)) = "male" then
               Data.Features (row - 1).Sex := 0;
            else
               Data.Features (row - 1).Sex := 1;
            end if;
            Next (Word_Cursor);              --  Age
            aWord := Element (Word_Cursor);
            if To_String (aWord)'Length = 0 then
               Data.Features (row - 1).Age := 50.0;
            else
               Data.Features (row - 1).Age := Float'Value (To_String (aWord));
            end if;
            Next (Word_Cursor);              --  SibSp
            Data.Features (row - 1).Sib_Sp :=
              Integer'Value (To_String (Element (Word_Cursor)));
            Next (Word_Cursor);              --  Parch
            Data.Features (row - 1).Parch :=
              Integer'Value (To_String (Element (Word_Cursor)));
            Next (Word_Cursor);              --  Ticket
            Next (Word_Cursor);
            Data.Features (row - 1).Fare :=
              Float'Value (To_String (Element (Word_Cursor)));
            Next (Word_Cursor);              --  Cabin
            Next (Word_Cursor);              --  Embarked
            if To_String (Element (Word_Cursor))'Length = 0 then
               Embarked := " ";
            else
               Embarked := To_String (Element (Word_Cursor));
            end if;
            Data.Features (row - 1).Embark_S := Embarked = "S";
            Data.Features (row - 1).Embark_C := Embarked = "C";
            Data.Features (row - 1).Embark_Q := Embarked = "Q";
         end loop;

         return Data;
      end;

   end Get_Data;

   --  -------------------------------------------------------------------------

   function Get_Split_Data (File_Name : String) return Split_Data_Record is
      Data         : constant Data_Record := Get_Data (File_Name);
      Mask         : Boolean_Array (1 .. Data.Num_Items);
      Train_Length : Natural := 0;
      Test_Length  : Natural := 0;
   begin
      for index in Mask'Range loop
         Mask (index) :=
           Maths.Random_Integer (0, 1) = 1;
         if Mask (index) then
            Train_Length := Train_Length + 1;
         else
            Test_Length := Test_Length + 1;
         end if;
      end loop;

      declare
         Train_Data : Data_Record (Train_Length);
         Test_Data  : Data_Record (Test_Length);
         Train_Row  : Natural := 0;
         Test_Row   : Natural := 0;
      begin
         for row in Mask'Range loop
            if Mask (row) then
               Train_Row := Train_Row + 1;
               Train_Data.Features (Train_Row) := Data.Features (row);
               Train_Data.Labels (Train_Row) := Data.Labels (row);
            else
               Test_Row := Test_Row + 1;
               Test_Data.Features (Test_Row) := Data.Features (row);
               Test_Data.Labels (Test_Row) := Data.Labels (row);
            end if;
         end loop;

         declare
            Split_Data : Split_Data_Record (Train_Length, Test_Length);
         begin
            Split_Data.Train_Features := Train_Data.Features;
            Split_Data.Train_Labels := Train_Data.Labels;
            Split_Data.Test_Features := Test_Data.Features;
            Split_Data.Test_Labels := Test_Data.Labels;

            return Split_Data;
         end;
      end;

   end Get_Split_Data;

   --  -------------------------------------------------------------------------

end Support_10A;
