
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;

package body Support_22A is

   function Set_Col_Names return ML_Types.Indef_String_List;

  --  -------------------------------------------------------------------------

   function Find_Item
     (Dictionary : Dictionary_List; Key : Unbounded_String;
      Item       : out Dictionary_Record) return Boolean is
      use Dictionary_Package;
      --        Routine_Name : constant String := "Support_6A.Find_Item ";
      Curs  : Cursor := Dictionary.First;
      Found : Boolean := False;

   begin
      while Has_Element (Curs) and not Found loop
         Item := Element (Curs);
         Found := Item.Key = Key;
         Next (Curs);
      end loop;

      return Found;

   end Find_Item;

   --  -------------------------------------------------------------------------

   function Get_Data (File_Name : String) return Data_Record is
      --        Routine_Name : constant String := "Support_22A.Get_Data ";
      File_ID         : File_Type;
      Data            : Data_Record;
   begin
      Data.Col_Names := Set_Col_Names;
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

   function Set_Col_Names return ML_Types.Indef_String_List is
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

   end Set_Col_Names;

   --  -------------------------------------------------------------------------

end Support_22A;
