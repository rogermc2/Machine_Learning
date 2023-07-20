
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Neural_Loader;

package body Support_22A is

   function Feature_Names return ML_Types.Indef_String_List;

  --  -------------------------------------------------------------------------
  --  Feature: treatment, y_factual, y_cfactual, mu0, mu1, x1 .. x25
   function Get_Data (File_Name : String) return Data_Record is
      --        use Neural_Loader;
      use ML_Types;
      use Raw_Data_Package;
      --        Routine_Name : constant String := "Support_22A.Get_Data ";
--        File_ID         : File_Type;
      Raw_Data        : constant ML_Types.Raw_Data_Vector :=
        Neural_Loader.Load_Raw_CSV_Data (File_Name);
      CSV_Line        : Unbounded_List;
      Value           : Unbounded_String;
      Features_Row    : Row_Record;
      Data            : Data_Record;
   begin
      Data.Col_Names := Feature_Names;
      for row in Raw_Data.First_Index .. Raw_Data.Last_Index loop
         CSV_Line := Raw_Data (row);
         Features_Row.Treatment :=
           Integer'Value (To_String (CSV_Line.First_Element)) = 1;
         for col in CSV_Line.First_Index + 1 .. CSV_Line.Last_Index loop
            Value := CSV_Line (col);
         end loop;
      end loop;
      --        Open (File_ID, In_File, File_Name);
--        while not End_Of_File (File_ID) loop
--           declare
--              aLine     : constant String := Get_Line (File_ID);
--              Treatment : constant Integer := Integer'Value (aLine (1 .. 1));
--              Token : constant Integer_Array :=
--                        Tokenize (aLine (3 .. aLine'Last), Dictionary);
--           begin
--              Row.Treatment := Treatment = 1;
--              Data.Labels.Append (Label);
--              Data.Features.Append (Token);
--           end;
--        end loop;
--
--        Close (File_ID);

      return Data;

   end Get_Data;

   --  -------------------------------------------------------------------------
   --  Treatment is a 0/1 variable that indicates the absence or presence of
   --  specialized treatment for each child.
   --  The feature labeled y_factual is an assessment of the child's
   --  improvements in cognitive development.
   --  The other features are not given semantically meaningful names to
   --  protect the privacy of the participants.
   function Feature_Names return ML_Types.Indef_String_List is
      use Ada.Strings;
      use ML_Types;
      use Indefinite_String_Package;
      --        Routine_Name : constant String := "Support_22A.Set_Col_Names ";
      X_String    : Unbounded_String;
      Cols        : ML_Types.Indef_String_List;
   begin
      Cols.Append ("treatment");
      Cols.Append ("y_factual");
      Cols.Append ("y_cfactual");
      Cols.Append ("mu0");
      Cols.Append ("mu1");
      for index in 1 .. 25 loop
         X_String := Trim (To_Unbounded_String (Integer'Image(index)), Both);
         Cols.Append ("x" & To_String (X_String));
      end loop;

      return Cols;

   end Feature_Names;

   --  -------------------------------------------------------------------------

end Support_22A;
