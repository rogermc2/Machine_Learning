
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
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
      use Ada.Strings.Unbounded;
      use ML_Types;
      use String_Package;
      Routine_Name  : constant String := "Support_8A.Get_Data ";
      Raw_Data      : Unbounded_List;
      Row_Words     : String_List;
      aWord         : Unbounded_String;
      Word_Cursor   : Cursor;
      Boolean_Value : Value_Record (Boolean_Type);
      Float_Value   : Value_Record (Float_Type);
      Integer_Value : Value_Record (Integer_Type);
      Unbound_Value : Value_Record (UB_String_Type);
   begin
      Raw_Data := Neural_Loader.Load_CSV_Data (File_Name);
      Put_Line (Routine_Name & File_Name & " loaded");

      declare
         Data_Length : constant Positive := Positive (Raw_Data.Length) - 1;
         Data        : Data_Record (Data_Length, 9);
         Embarked    : String (1 .. 1);
      begin
         Data.Feature_Names :=
           (To_Unbounded_String ("Pclass"), To_Unbounded_String ("Sex"),
            To_Unbounded_String ("Age"), To_Unbounded_String ("SibSp"),
            To_Unbounded_String ("Parch"), To_Unbounded_String ("Fare"),
            To_Unbounded_String ("Embarked S"),
            To_Unbounded_String ("Embarked C"),
            To_Unbounded_String ("Embarked Q"));

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
            Integer_Value.Integer_Value :=
              Integer'Value (To_String (Element (Word_Cursor)));
            Data.Features (row - 1, 1) := Integer_Value;
            Next (Word_Cursor);              --  Name
            Next (Word_Cursor);              --  Sex
            Unbound_Value.UB_String_Value := Element (Word_Cursor);
            Data.Features (row - 1, 2) := Unbound_Value;
            Next (Word_Cursor);              --  Age
            Integer_Value.Integer_Value :=
              Integer'Value (To_String (Element (Word_Cursor)));
            Data.Features (row - 1, 3) := Integer_Value;
            Next (Word_Cursor);              --  SibSp
            Integer_Value.Integer_Value :=
              Integer'Value (To_String (Element (Word_Cursor)));
            Data.Features (row - 1, 4) := Integer_Value;
            Next (Word_Cursor);              --  Parch
            Integer_Value.Integer_Value :=
              Integer'Value (To_String (Element (Word_Cursor)));
            Data.Features (row - 1, 5) := Integer_Value;
            Next (Word_Cursor);              --  Ticket
            Next (Word_Cursor);              --  Fare
            Float_Value.Float_Value :=
              Float'Value (To_String (Element (Word_Cursor)));
            Data.Features (row - 1, 6) := Float_Value;
            Next (Word_Cursor);              --  Cabin
            Next (Word_Cursor);              --  Embarked
            Embarked := To_String (Element (Word_Cursor));
            Boolean_Value.Boolean_Value := Embarked = "S";
            Data.Features (row - 1, 7) := Boolean_Value;
            Boolean_Value.Boolean_Value := Embarked = "C";
            Data.Features (row - 1, 8) := Boolean_Value;
            Boolean_Value.Boolean_Value := Embarked = "Q";
            Data.Features (row - 1, 9) := Boolean_Value;

         end loop;

         return Data;
      end;

--        declare
--           Data  : Data_Record (Integer (Features.Length),
--                                Integer (Features (1).Length));
--        begin
--           Data.Features := To_Real_Float_Matrix (Features);
--           Data.Labels := To_Integer_Array (Labels);
--           return Data;
--        end;

   end Get_Data;

   --  -------------------------------------------------------------------------

end Support_10A;
