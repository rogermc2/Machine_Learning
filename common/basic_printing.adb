
with Interfaces;

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

package body Basic_Printing is

   procedure Print_Binary_Matrix
     (Name  : String; aMatrix : Binary_Matrix;
      Start : Positive := 1; Finish : Natural := 0) is
      Last : Positive;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (aMatrix'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= aMatrix'First and then Finish <= aMatrix'Last then
         for row in Start .. Last loop
            for col in aMatrix'Range (2) loop
               Put (Integer'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
         end loop;
      else
         Put_Line ("Print_Binary_Matrix called with invalid start or finish index.");
      end if;
      New_Line;

   end Print_Binary_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Boolean_List (Name  : String;
                                 aList : NL_Types.Boolean_List) is
      Count : Positive := 1;
   begin
      Put_Line (Name & ": ");
      for Index in aList.First_Index .. aList.Last_Index loop
         Put (Boolean'Image (aList (Index)) & "  ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Boolean_List;

   --  ------------------------------------------------------------------------

   procedure Print_Byte_Array (Name  : String; anArray : Byte_Array;
                               Start : Positive := 1; Finish : Natural := 0) is
      Last  : Positive;
      Count : Positive := 1;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (anArray'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= anArray'First and then Finish <= anArray'Last then
         for Index in Start .. Last loop
            Put (Interfaces.Unsigned_8'Image (anArray (Index)) & "  ");
            Count := Count + 1;
            if Count > 4 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Print_Byte_Array called with invalid start or finish index.");
      end if;
      New_Line;

   end Print_Byte_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Array (Name  : String; anArray : Real_Float_Vector;
                                Start : Positive := 1; Finish : Natural := 0) is
      Last  : Positive;
      Count : Positive := 1;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (anArray'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= anArray'First and then Finish <= anArray'Last then
         for Index in Start .. Last loop
            Put (Float'Image (anArray (Index)) & "  ");
            Count := Count + 1;
            if Count > 4 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Print_Float_Array called with invalid start or finish index.");
      end if;
      New_Line;

   end Print_Float_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Matrix
     (Name      : String; aMatrix : Real_Float_Matrix;
      Start     : Positive := 1; Finish : Natural := 0;
      Col_Start : Positive := 1; Col_Finish : Natural := 0) is
      Last     : Positive;
      Col_Last : Positive;
   begin
      if Finish > 0 and then Finish < aMatrix'Length then
         Last := Finish;
      else
         Last := Integer (aMatrix'Length);
      end if;

      if Col_Finish > 0 and then Col_Finish < aMatrix'Length (2) then
         Col_Last := Col_Finish;
      else
         Col_Last := Integer (aMatrix'Length (2));
      end if;

      Put_Line (Name & ": ");
      if Start >= aMatrix'First and Col_Start >= aMatrix'First (2) then
         for row in Start .. Last loop
            for col in Col_Start .. Col_Last loop
               Put (Float'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
         end loop;

      else
         Put_Line ("Print_Float_Matrix called with invalid start index.");
      end if;
      New_Line;

   end Print_Float_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Matrix_List
     (Name  : String; aList : Real_Matrix_List;
      Start : Positive := 1; Finish : Natural := 0) is
   begin
      Put_Line (Name);
      for index in aList.First_Index .. aList.Last_Index loop
         Print_Float_Matrix ("" & Integer'Image (index),
                             aList (index), Start, Finish);
      end loop;

   end Print_Float_Matrix_List;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Vector
     (Name  : String; Vec : Real_Float_Vector;
      Start : Positive := 1; Finish : Natural := 0) is
      Last  : Integer;
      Count : Positive := 1;
   begin
      if Finish > 0 and then Finish < Vec'Length then
         Last := Finish;
      else
         Last := Integer (Vec'Length);
      end if;

      if Name'Length > 0 then
         Put_Line (Name & ": ");
      end if;

      if Start >= Vec'First then
         for row in Start .. Last loop
            Put (Float'Image (Vec (row)) & "  ");
         end loop;

         Count := Count + 1;
         if Count > 8 then
            New_Line;
            Count := 1;
         end if;

      else
         Put_Line ("Print_Float_Vector called with invalid start index.");
      end if;
      New_Line;

   end Print_Float_Vector;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Vector_As_Line
     (Name  : String; Vec : Real_Float_Vector;
      Start : Positive := 1; Finish : Natural := 0) is
      Last  : Integer;
      Count : Positive := 1;
   begin
      if Finish > 0 and then Finish < Vec'Length then
         Last := Finish;
      else
         Last := Integer (Vec'Length);
      end if;

      if Name'Length > 0 then
         Put ("  " & Name & ": ");
      end if;

      if Start >= Vec'First then
         for row in Start .. Last loop
            Put (Float'Image (Vec (row)) & "  ");
         end loop;

         Count := Count + 1;
         if Count > 8 then
            New_Line;
            Count := 1;
         end if;

      else
         Put_Line ("Print_Float_Vector_As_Line called with invalid start index.");
      end if;
      New_Line;

   end Print_Float_Vector_As_Line;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Array
     (Name  : String; anArray : Integer_Array;
      Start : Positive := 1; Finish : Natural := 0) is
      Last  : Positive;
      Count : Positive := 1;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (anArray'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= anArray'First and then Finish <= anArray'Last then
         for Index in Start .. Last loop
            Put (Integer'Image (anArray (Index)) & "  ");
            Count := Count + 1;
            if Count > 10 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Print_Integer_Array called with invalid start or finish index.");
      end if;
      New_Line;

   end Print_Integer_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Array_List
     (Name  : String; aList : Integer_Array_List;
      List_Start : Positive := 1; List_Finish : Natural := 0;
      Start : Positive := 1; Finish : Natural := 0) is
      List_Last  : Positive;
   begin
      Put_Line (Name);
      if List_Finish > 0 then
         List_Last := List_Finish;
      else
         List_Last := Integer (aList.Length);
      end if;

      for index in List_Start .. List_Last loop
         Print_Integer_Array ("" & Integer'Image (index),
                              aList (index), Start, Finish);
      end loop;

   end Print_Integer_Array_List;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_List
     (Name  : String; aList : ML_Types.Integer_List;
      Start : Positive := 1; Finish : Natural := 0) is
      Last  : Positive;
      Count : Integer := 1;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (aList.Length);
      end if;

      Put_Line (Name & ": ");
      for Index in Start .. Last loop
         Put (Integer'Image (aList (Index)) & "  ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Integer_List;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Map
     (Name : String; aMap : ML_Types.Integer_Label_Map) is
      use ML_Types.Integer_Label_Map_Package;
      Curs : Cursor := aMap.First;
      aKey : Integer;
   begin
      Put_Line (Name & ": ");
      while Has_Element (Curs) loop
         aKey := Key (Curs);
         Put_Line (Integer'Image (aKey) & ":  " &
                     Integer'Image (Element (Curs)));
         Next (Curs);
      end loop;

   end Print_Integer_Map;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Matrix
     (Name  : String; aMatrix : Integer_Matrix;
      Start : Natural := 0; Finish : Integer := 0) is
      First : Integer := aMatrix'First;
      Last  : Integer;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := aMatrix'Last;
      end if;

      if Start > First and Start <= Last then
         First := Start;
      end if;

      Put_Line (Name & ": ");
      if Finish <= aMatrix'Last then
         for row in First .. Last loop
            for col in aMatrix'Range (2) loop
               Put (Integer'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
         end loop;

      else
         Put_Line ("Print_Integer_Matrix called with invalid start or " &
                     "finish index.");
      end if;

   end Print_Integer_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_List_Dimensions (Name : String; aList : Real_Float_List) is
      use Ada.Containers;
   begin
      Put (Name & " length: ");
      Put_Line (Count_Type'Image (aList.Length));

   end Print_List_Dimensions;

   --  ------------------------------------------------------------------------

   procedure Print_List_Dimensions (Name  : String;
                                    aList : Real_Float_List_2D) is
      use Ada.Containers;
   begin
      Put (Name & " size: ");
      Put_Line (Count_Type'Image (aList.Length) & "  x" &
                  Count_Type'Image (aList (1).Length));

   end Print_List_Dimensions;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Binary_Matrix) is
   begin
      Put (Name & ": ");
      Put_Line (Integer'Image (aMatrix'Length) & "  x" &
                  Integer'Image (aMatrix'Length (2)));

   end Print_Matrix_Dimensions;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Integer_Matrix) is
   begin
      Put (Name & " dimensions:");
      Put_Line (Integer'Image (aMatrix'Length) & "  x" &
                  Integer'Image (aMatrix'Length (2)));

   end Print_Matrix_Dimensions;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Real_Float_Matrix) is
   begin
      Put (Name & " dimensions:");
      Put_Line (Integer'Image (aMatrix'Length) & "  x" &
                  Integer'Image (aMatrix'Length (2)));

   end Print_Matrix_Dimensions;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Unsigned_8_Array_3D) is
   begin
      Put (Name & " dimensions:");
      Put_Line (Integer'Image (aMatrix'Length) & "  x" &
                  Integer'Image (aMatrix'Length (2)) & "  x" &
                  Integer'Image (aMatrix'Length (3)));

   end Print_Matrix_Dimensions;

   --  ------------------------------------------------------------------------

   procedure Print_Real_Float_List
     (Name  : String; aList : Real_Float_List;
      Start : Positive := 1; Finish : Natural := 0) is
      Routine_Name : constant String := "Print_Real_Float_List ";
      Last         : Positive;
      Count        : Positive := 1;
   begin
      if Integer (aList.Length) > 0 then
         if Finish > 0 and Finish <= aList.Last_Index then
            Last := Finish;
         else
            Last := Integer (aList.Last_Index);
         end if;

         Put_Line (Name & ": ");
         if Start >= aList.First_Index and Last >= Start then
            for Index in Start .. Last loop
               Put (Float'Image (aList (Index)) & "  ");
               Count := Count + 1;
               if Count > 10 then
                  New_Line;
                  Count := 1;
               end if;
            end loop;
         else
            Put_Line (Routine_Name & "called with invalid start index.");
         end if;
      else
         Put_Line (Routine_Name & "called with empty list");
      end if;
      New_Line;

   end Print_Real_Float_List;

   --  ------------------------------------------------------------------------

   procedure Print_Real_Float_List_2D
     (Name  : String; aList : Real_Float_List_2D;
      Start : Positive := 1; Finish : Natural := 0) is
      Routine_Name : constant String := "Print_Real_Float_List_2D ";
      Row_List     : Real_Float_List;
      Last         : Integer;
      Count        : Positive;
   begin
      if Integer (aList.Length) > 0 then
         if Finish > 0 and Finish <= aList.Last_Index then
            Last := Finish;
         else
            Last := Integer (aList.Last_Index);
         end if;

         Put_Line (Name & ": ");
         if Start >= aList.First_Index and Last >= Start then
            for row in Start .. Last loop
               Row_List := aList (row);
               Count := 1;
               for col in Row_List.First_Index .. Row_List.Last_Index loop
                  Put (Float'Image (Row_List (col)) & "  ");
                  Count := Count + 1;
                  if Count > 10 and col /= Row_List.Last_Index then
                     New_Line;
                     Count := 1;
                  end if;
               end loop;
               New_Line;
            end loop;
         elsif Start < aList.First_Index then
            Put_Line (Routine_Name & "called with invalid start index:" &
                        Integer'Image (Start));
         else
            Put_Line (Routine_Name & "called with invalid Last index:" &
                        Integer'Image (Last));
         end if;
      else
         Put_Line (Routine_Name & "called with empty list");
      end if;
      New_Line;

   end Print_Real_Float_List_2D;

   --  ------------------------------------------------------------------------

   procedure Print_Unbound_Array (Name : String;
                                  UB   : Unbounded_String_Array) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in UB'Range loop
         Put (To_String (UB (Index)) & "  ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Unbound_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Unbound_List (Name : String;
                                 UB   : ML_Types.Unbounded_List) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in UB.First_Index .. UB.Last_Index loop
         Put (To_String (UB (Index)) & "  ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Unbound_List;

   --  ------------------------------------------------------------------------

   procedure Print_String_Map (Name : String; aMap : ML_Types.String_Map;
                                Start : Natural := 0; Finish : Natural := 0) is
      use ML_Types.String_Map_Package;
      Curs  : Cursor := aMap.First;
      Last  : Natural;
      Count : Natural := 0;
   begin
      if Integer (aMap.Length) > 0 then
         if Finish > 0 and Finish <= Natural (aMap.Length) then
            Last := Finish;
         else
            Last := Integer (aMap.Length);
         end if;

         while Has_Element (Curs) and Count <= Start loop
            Count := Count + 1;
            Next (Curs);
         end loop;

         Put_Line (Name & ": ");
         while Has_Element (Curs) and Count < Last loop
            Count := Count + 1;
            declare
               aKey : constant String := Key (Curs);
            begin
               Put_Line (aKey & ": " & Integer'Image (Element (Curs)));
            end;
            Next (Curs);
         end loop;
      end if;

   end Print_String_Map;

   --  ------------------------------------------------------------------------

   procedure Print_Unbound_Matrix (Name : String;
                                   UB   : Unbounded_String_Matrix) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for row in UB'Range loop
         for col in UB'Range (2) loop
            Put (To_String (UB (row, col)) & "  ");
            Count := Count + 1;
            if Count > 10 then
               New_Line;
               Count := 1;
            end if;
         end loop;
         New_Line;
      end loop;

   end Print_Unbound_Matrix;

   --  ------------------------------------------------------------------------

end Basic_Printing;
