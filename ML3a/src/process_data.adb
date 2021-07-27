
with Ada.Containers;
with Ada.Strings.Unbounded;

with Utilities;

with ML_Types;

package body Process_Data is

   --  -------------------------------------------------------------------------

   function Data_Length (Data : Data_Record) return Integer is
   begin
      return Data.Labels.Last_Index;
   end Data_Length;

   --  -------------------------------------------------------------------------

   procedure Load_Data (Data_File : File_Type; Num_Features : Integer;
                        Data : out Data_Record) is

      use Ada.Containers;
      use Ada.Strings.Unbounded;
      use ML_Types.String_Package;
      Values      : Values_Array (1 .. Num_Features);
      CSV_Line    : ML_Types.String_List;
      Label       : Integer;
      Curs        : ML_Types.String_Package.Cursor;
      Value_Index : Integer := 0;
      Label_String : String (1 .. 1);
   begin
      while not End_Of_File (Data_File) loop
         declare
            Data_Line : constant String := Get_Line (Data_File);
         begin
            CSV_Line := Utilities.Split_String (Data_Line, ",");
            Curs := CSV_Line.First;
            Value_Index := 0;
            while Has_Element (Curs) loop
               if Curs /= CSV_Line.Last then
                  Value_Index := Value_Index + 1;
                  Values (Value_Index) := Float'Value (To_String (Element (Curs)));
               else
                  Label_String := To_String (Element (Curs)) (1 .. 1);
                  Label := Integer'Value (Label_String);
               end if;
               Next (Curs);
            end loop;
            Data.Data.Append (Values);
            Data.Labels.Append (Label);
         end;
      end loop;
      Put_Line ("Data length: " & Count_Type'Image (Data.Data.Length));
      Print_Data_Item (Data.Data, Num_Features, 15);
--        Print_Data (Data.Data, Num_Features);

   end Load_Data;

   --  -------------------------------------------------------------------------

   procedure Print_Data (Data        : Data_List; Num_Features : Integer;
                         First       : Integer := 1; Last : Integer := 10) is
      use Data_Package;
      Data_Length : constant Integer := Data.Last_Index;
      Data_Curs   : Data_Package.Cursor := Data.First;
      Values      : Values_Array (1 .. Num_Features);
      Data_Count  : Integer := 0;
   begin
      Put_Line ("Data");
      while Has_Element (Data_Curs) and Data_Count < Last loop
         Data_Count := Data_Count + 1;
         --           Put_Line ("Data item " & Integer'Image (Data_Count));
               Values := Element (Data_Curs);
               for index in Values'Range loop
                  Put (Float'Image (Values (index)));
                  if index /= Data_Length then
                     Put (", ");
                  end if;
               end loop;
               New_Line;
         Next  (Data_Curs);
      end loop;
   end Print_Data;

   --  -------------------------------------------------------------------------

   procedure Print_Data_Item (Data : Data_List;  Num_Features : Integer;
                              Item : Integer) is
      Values : constant Values_Array (1 .. Num_Features) := Data.Element (Item);
   begin
      Put_Line ("Data item " & Positive'Image (Item));
         for index in Values'Range loop
            Put (Float'Image (Values (index)));
            if index /= Num_Features then
               Put (", ");
            end if;
         end loop;
         New_Line;

   end Print_Data_Item;

   --  -------------------------------------------------------------------------

end Process_Data;
