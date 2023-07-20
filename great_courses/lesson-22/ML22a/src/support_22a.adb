
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;

package body Support_22A is

   function Feature_Names return ML_Types.Indef_String_List;

  --  -------------------------------------------------------------------------

   function Get_Data (File_Name : String) return Data_Record is
      --        Routine_Name : constant String := "Support_22A.Get_Data ";
      File_ID         : File_Type;
      Data            : Data_Record;
   begin
      Data.Col_Names := Feature_Names;
      Open (File_ID, In_File, File_Name);
      while not End_Of_File (File_ID) loop
         declare
            aLine : constant String := Get_Line (File_ID);
            Label : constant Integer := Integer'Value (aLine (1 .. 1));
            Token : constant Integer_Array :=
                      Tokenize (aLine (3 .. aLine'Last), Dictionary);
         begin
            Data.Labels.Append (Label);
            Data.Features.Append (Token);
         end;
      end loop;

      Close (File_ID);
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
