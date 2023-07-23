
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Neural_Loader;

package body Support_Do_Why is

   function Feature_Names return ML_Types.Indef_String_List;
   procedure Print_Feature_Names (Names : ML_Types.Indef_String_List);

   --  -------------------------------------------------------------------------
   --  Feature: treatment, y_factual, y_cfactual, mu0, mu1, x1 .. x25
   function Get_Data (File_Name : String) return Data_Record is
      use ML_Types;
      use Raw_Data_Package;
--        Routine_Name : constant String := "Support_22A.Get_Data ";
      Raw_Data        : constant ML_Types.Raw_Data_Vector :=
                          Neural_Loader.Load_Raw_CSV_Data (File_Name);
      CSV_Line        : Unbounded_List;
      Features_Row    : Row_Record;
      Data            : Data_Record;
   begin
      Data.Col_Names := Feature_Names;
--        Print_Indefinite_List (Routine_Name & "Data.Col_Names", Data.Col_Names);
      for row in Raw_Data.First_Index .. Raw_Data.Last_Index loop
         CSV_Line := Raw_Data (row);
         Features_Row.Treatment :=
           Integer'Value (To_String (CSV_Line.First_Element)) = 1;
         for col in Features_Row.Float_Data'Range loop
            Features_Row.Float_Data (col) :=
              Float'Value (To_String (CSV_Line (col)));
         end loop;

         for col in Features_Row.X7_25'Range loop
            Features_Row.X7_25 (col) :=
              Integer'Value (To_String (CSV_Line (col))) = 1;
         end loop;
         Data.Data.Append (Features_Row);
      end loop;

      return Data;

   end Get_Data;

   --  -------------------------------------------------------------------------
   --  Treatment is a 0/1 variable that indicates the absence or presence of
   --  specialized treatment for each child.
   --  The feature labeled y_factual is an assessment of the childs
   --  improvements in cognitive development.
   --  The other features are not given semantically meaningful names to
   --  protect the privacy of the participants.
   function Feature_Names return ML_Types.Indef_String_List is
      use Ada.Strings;
      use ML_Types.Indefinite_String_Package;
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

   function Get_X_Names (Names : ML_Types.Indef_String_List)
                         return Unbounded_String is
      use  ML_Types.Indefinite_String_Package;
      Curs   : Cursor := Names.First;
      Name   : Unbounded_String;
      Count  : Natural := 0;
      Result : Unbounded_String;
   begin
      New_Line;
      while Has_Element (Curs) loop
         Count := Count + 1;
         if Count > 5 then
            Name := To_Unbounded_String (Element (Curs));
            Result := Result & Name;
            if Count < Integer (Names.Length) then
               Result := Result & To_Unbounded_String ("+");
            end if;
         end if;
         Next (Curs);
      end loop;

      return Result;

   end Get_X_Names;

   --  ------------------------------------------------------------------------

   procedure Print_Data (theList : Data_Record; Start : Positive := 1;
                         Finish  : Natural := 0) is
      use Data_Package;
      Curs      : Cursor := theList.Data.First;
      Data_Row  : Row_Record;
      Last_Item : Positive;
      Count     : Natural := 0;
   begin
      Print_Feature_Names (theList.Col_Names);
      if Finish > 0 then
         Last_Item := Finish;
      else
         Last_Item := Integer (theList.Data.Length);
      end if;

      while Has_Element (Curs) loop
         Count := Count + 1;
         if Count >= Start and Count <= Last_Item then
            Data_Row := theList.Data (Curs);
            Put (Boolean'Image (Data_Row.Treatment));
            for col in Data_Row.Float_Data'Range loop
               Put (Float'Image (Data_Row.Float_Data (col)) & "  ");
            end loop;
            for col in Data_Row.X7_25'Range loop
               Put (Boolean'Image (Data_Row.X7_25 (col)) & "  ");
            end loop;
            New_Line;
         end if;
         Next (Curs);
      end loop;

   end Print_Data;

   --  ------------------------------------------------------------------------

   procedure Print_Feature_Names (Names : ML_Types.Indef_String_List) is
      use  ML_Types.Indefinite_String_Package;
      Curs : Cursor := Names.First;
   begin
      New_Line;
      while Has_Element (Curs) loop
         Put (Element (Curs) & " ");
         Next (Curs);
      end loop;
      New_Line;

   end Print_Feature_Names;

   --  ------------------------------------------------------------------------

end Support_Do_Why;
