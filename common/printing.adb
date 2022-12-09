
with Ada.Text_IO; use Ada.Text_IO;

package body Printing is

   procedure Print_Array_Of_Integer_Lists
     (Name  : String; theArray : ML_Types.Array_Of_Integer_Lists;
      Start : Integer := 1; Finish : Integer := 0) is
      Last  : Integer;
      aList : ML_Types.Integer_List;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (theArray'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= theArray'First and then Finish <= theArray'Last then
         for row in Start .. Last loop
            aList := theArray (row);
            for col in aList.First_Index .. aList.Last_Index loop
               Put (Integer'Image (aList (col)) & "  ");
            end loop;
            New_Line;
         end loop;
      else
         Put_Line
           ("Print_Array_Of_Integer_Lists called with invalid start or finish index.");
      end if;

   end Print_Array_Of_Integer_Lists;

   --  ------------------------------------------------------------------------

   procedure Print_Indefinite_List (Name    : String;
                                    theList : ML_Types.Indef_String_List) is
      use  ML_Types.Indefinite_String_Package;
      Curs : Cursor := theList.First;
   begin
      Put_Line (Name & ": ");
      while Has_Element (Curs) loop
         Put_Line (Element (Curs));
         Next (Curs);
      end loop;
      New_Line;

   end Print_Indefinite_List;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_List (Name    : String;
                                 theList : ML_Types.Integer_List;
                                 Start : Positive := 1; Last : Positive := 10)
   is
      Count : Integer := 1;
      Stop  : Integer := Last;
   begin
      if Stop > Integer (theList.Length) then
         Stop := Integer (theList.Length);
      end if;
      Put_Line (Name & ": ");
      for Index in Start .. Stop loop
         Put (Integer'Image (theList.Element (Index)) & "  ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Integer_List;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_List (Name    : String;
                                 theList : ML_Types.Integer_DL_List) is
      use ML_Types.Integer_DLL_Package;
      Curs  : Cursor := theList.First;
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      while Has_Element (Curs) loop
         Put (Integer'Image (Element (Curs)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
         Next (Curs);
      end loop;
      New_Line;

   end Print_Integer_List;

   --  ------------------------------------------------------------------------

   procedure Print_Strings (Name : String; theList : ML_Types.String_List) is
      use ML_Types.String_Package;
      Curs  : Cursor := theList.First;
      Count : Integer := 1;
   begin
      if Name'Length > 0 then
         Put (Name & ": ");
      end if;

      while Has_Element (Curs) loop
         Put (To_String (Element (Curs)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
         Next (Curs);
      end loop;
      New_Line;

   end Print_Strings;

   --  ------------------------------------------------------------------------

   procedure Print_Strings (Name    : String;
                            theList : ML_Types.Indef_String_List) is
      use ML_Types.Indefinite_String_Package;
      Curs  : Cursor := theList.First;
      Count : Integer := 1;
   begin
      if Name'Length > 0 then
         Put (Name & ": ");
      end if;
      Put_Line ("Length: " & Integer'Image (Integer (theList.Length)));

      while Has_Element (Curs) loop
         Put (Element (Curs) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
         Next (Curs);
      end loop;
      New_Line;

   end Print_Strings;

   --  ------------------------------------------------------------------------

   procedure Print_Strings (Name : String; theList : ML_Types.String_Vector) is
      Count : Integer := 1;
   begin
      if Name'Length > 0 then
         Put (Name & ": ");
      end if;

      for index in theList.First_Index .. theList.Last_Index loop
         Put (To_String (theList.Element (index)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Strings;

   --  ------------------------------------------------------------------------

   procedure Print_Unbounded_List (Name    : String;
                                   theList : ML_Types.Unbounded_List) is
      Count : Integer := 1;
   begin
      if Name'Length > 0 then
         Put (Name & ": ");
      end if;

      for Index in theList.First_Index .. theList.Last_Index loop
         Put (To_String (theList.Element (Index)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Unbounded_List;

   --  ------------------------------------------------------------------------

   procedure Print_Value_Data_List (Name    : String;
                                    theList : ML_Types.Value_Data_List) is
      Value : ML_Types.Value_Record;
      Count : Integer := 1;
   begin
      if Name'Length > 0 then
         Put_Line (Name);
      end if;

      Put_Line ("List length: " & Integer'Image (Integer (theList.Length)));
      for Index in theList.First_Index .. theList.Last_Index loop
         Value := theList.Element (Index);
         Put ("   ");
         Print_Value_Record ("", Value);
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Value_Data_List;

   --  ------------------------------------------------------------------------

   procedure Print_Value_Data_Lists_2D
     (Name      : String; theList : ML_Types.Value_Data_Lists_2D;
      Num_Items : Positive := 1000) is
      Items : Positive;
   begin
      if Name'Length > 0 then
         Put_Line (Name);
      end if;

      if Integer (theList.Length) = 0 then
         Put_Line ("List is empty");

      elsif Integer (theList.Element (1).Length) = 0 then
         Put_Line ("First data list is empty");

      else
         Items := Positive (theList.Last_Index);
         if Items > Num_Items then
            Items := Num_Items;
         end if;

         for index in theList.First_Index .. Items loop
            Print_Value_Data_List (Integer'Image (index) & ":",
                                   theList.Element (index));
         end loop;
      end if;

   end Print_Value_Data_Lists_2D;

   --  ------------------------------------------------------------------------

   procedure Print_Value_Data_Lists_3D
     (Name : String; theList : ML_Types.Value_Data_Lists_3D) is
   begin
      if Name'Length > 0 then
         Put_Line (Name & ":");
      end if;

      if Integer (theList.Length) = 0 then
         Put_Line ("Print_Value_Data_List_3D list is empty");
      elsif Integer (theList.Element (1).Length) = 0 then
         Put_Line ("Print_Value_Data_List_3D, first 2D list is empty");
      else
         for index in theList.First_Index .. theList.Last_Index loop
            Print_Value_Data_Lists_2D ("", theList.Element (index));
         end loop;
      end if;

   end Print_Value_Data_Lists_3D;

   --  -------------------------------------------------------------

   procedure Print_Value_Record
     (Name : String; Value : ML_Types.Value_Record) is
      use ML_Types;
   begin
      if Name'Length > 0 then
         Put_Line (Name & ":");
      end if;

      case Value.Value_Kind is
         when Boolean_Type => Put (Boolean'Image (Value.Boolean_Value));
         when Float_Type => Put (Float'Image (Value.Float_Value));
         when Integer_Type => Put (Integer'Image (Value.Integer_Value));
         when UB_String_Type => Put (To_String (Value.UB_String_Value));
      end case;

      if Name'Length > 0 then
         New_Line;
      end if;

   end Print_Value_Record;

   --  ------------------------------------------------------------------------

end Printing;
