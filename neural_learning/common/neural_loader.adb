
with Ada.Containers;
with Ada.Strings.Fixed;

package body Neural_Loader is

   function Split_String (aString, Pattern : String) return String_List;

   --  -------------------------------------------------------------------------

   function Load_CSV_Data (File_Name : String) return Unbounded_List is
      Data_File : File_Type;
      Data      : Unbounded_List;
   begin
      Open (Data_File, In_File, File_Name);
      Data := Load_CSV_Data (Data_File);
      Close (Data_File);

      return Data;

   end Load_CSV_Data;

   --  -------------------------------------------------------------------------

   function Load_CSV_Data (Data_File : File_Type) return Unbounded_List is
      Data : Unbounded_List;
   begin
      while not End_Of_File (Data_File) loop
         Data.Append (To_Unbounded_String (Get_Line (Data_File)));
      end loop;

      return Data;

   end Load_CSV_Data;

   --  -------------------------------------------------------------------------

   function Load_Raw_CSV_Data (File_Name : String;
                               Max_Lines : Positive := 20000)
                               return Raw_Data_Vector is
      Data_File : File_Type;
      Data      : Raw_Data_Vector;
   begin
      Put_Line ("Loading " & File_Name & " CSV Data");
      Open (Data_File, In_File, File_Name);
      Data := Load_Raw_CSV_Data (Data_File, Max_Lines);
      Close (Data_File);

      return Data;

   end Load_Raw_CSV_Data;

   --  -------------------------------------------------------------------------

   function Load_Raw_CSV_Data (Data_File : File_Type;
                               Max_Lines : Positive := 20000)
                               return Raw_Data_Vector is
      use String_Package;
      Data_Line : Unbounded_String;
      CSV_Line  : String_List;
      Curs      : String_Package.Cursor;
      Num_Lines : Natural := 0;
      Values    : Unbounded_List;
      Data      : Raw_Data_Vector;
   begin
      while not End_Of_File (Data_File) and Num_Lines <= Max_Lines loop
         Data_Line := To_Unbounded_String (Get_Line (Data_File));
         Num_Lines := Num_Lines + 1;
         CSV_Line := Split_String (To_String (Data_Line), ",");
         Curs := CSV_Line.First;
         Values.Clear;
         while Has_Element (Curs) loop
            Values.Append (Element (Curs));
            Next (Curs);
         end loop;
         Data.Append (Values);
      end loop;

      return Data;

   end Load_Raw_CSV_Data;

   --  -------------------------------------------------------------------------

   function Split_String (aString, Pattern : String) return String_List is
      Last       : constant Integer := aString'Last;
      Last_Char  : constant Character := aString (Last);
      UB_String  : Unbounded_String;
      Split_List : String_List;
   begin
      if Character'Pos (Last_Char) < 32 then
         UB_String :=
           To_Unbounded_String (aString (aString'First .. Last - 1));
      else
         UB_String := To_Unbounded_String (aString);
      end if;

      declare
         use Ada.Strings;
         String_2 : constant String := To_String (UB_String);
         Last_2   : constant Integer := String_2'Last;
         A_Index  : Integer;
         B_Index  : Integer := String_2'First;
      begin
         for index in String_2'First .. Fixed.Count (String_2, Pattern) loop
            A_Index :=
              Fixed.Index (String_2 (B_Index .. Last_2), Pattern);
            --  process string slice in any way
            Split_List.Append
              (To_Unbounded_String (String_2 (B_Index .. A_Index - 1)));
            B_Index := A_Index + Pattern'Length;
         end loop;
         --  process last string
         Split_List.Append
           (To_Unbounded_String (String_2 (B_Index .. Last_2)));
      end;
      return Split_List;

   end Split_String;

   --  -------------------------------------------------------------------------

end Neural_Loader;
