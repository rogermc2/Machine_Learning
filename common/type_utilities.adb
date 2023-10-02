
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

package body Type_Utilities is

   use ML_Types;

   function To_Array (L : ML_Types.Integer_List) return Integer_Array is
      New_Array : Integer_Array (1 .. Integer (L.Length));
      A_Index   : Integer := 0;
   begin
      for index in L.First_Index .. L.Last_Index loop
         A_Index := A_Index + 1;
         New_Array (A_Index) := L.Element (index);
      end loop;
      return New_Array;
   end To_Array;

   --  -------------------------------------------------------------------------

   function To_Float_List (A : Float_Array) return NL_Types.Float_List is
      A_List : NL_Types.Float_List;
   begin
      for index in A'First .. A'Last loop
         A_List.Append (A (index));
      end loop;
      return A_List;
   end To_Float_List;

   --  -------------------------------------------------------------------------

   function To_Float_List (F : ML_Types.Value_Data_List)
                           return NL_Types.Float_List is
      Item   : Value_Record;
      Floats : NL_Types.Float_List;
   begin
      for index in F.First_Index .. F.Last_Index loop
         Item := F.Element (index);
         Assert (Item.Value_Kind = Float_Type,
                 "Type_Utilities.To_Float_List invalid item "
                 & Integer'Image (index) & " data type is " &
                   Data_Type'Image (Item.Value_Kind));
         Floats.Append (Item.Float_Value);
      end loop;

      return Floats;

   end To_Float_List;

   --  -------------------------------------------------------------------------

   function To_Float_List (I : ML_Types.Integer_List)
                           return NL_Types.Float_List is
      F_List : NL_Types.Float_List;
   begin
      for index in I.First_Index .. I.Last_Index loop
         F_List.Append (Float (I.Element (index)));
      end loop;

      return F_List;

   end To_Float_List;

   --  -------------------------------------------------------------------------

   function To_Float_List_2D (Data : ML_Types.Value_Data_Lists_2D)
                              return NL_Types.Float_List_2D is
      F2_List : NL_Types.Float_List_2D;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         F2_List.Append (To_Float_List (Data (index)));
      end loop;

      return F2_List;

   end To_Float_List_2D;

   --  -------------------------------------------------------------------------

   function To_Float_List_2D (I : ML_Types.Integer_List_2D)
                              return NL_Types.Float_List_2D is
      F2_List : NL_Types.Float_List_2D;
   begin
      for index in I.First_Index .. I.Last_Index loop
         F2_List.Append (To_Float_List (I (index)));
      end loop;

      return F2_List;

   end To_Float_List_2D;

   --  -------------------------------------------------------------------------

   function To_Integer_List (A : Integer_Array)
                             return ML_Types.Integer_List is
      A_List : ML_Types.Integer_List;
   begin
      for index in A'First .. A'Last loop
         A_List.Append (A (index));
      end loop;
      return A_List;

   end To_Integer_List;

   --  -------------------------------------------------------------------------

   function To_Integer_List (Ints : ML_Types.Value_Data_List)
                             return Integer_List is
      Item   : Value_Record;
      Values : Integer_List;
   begin
      for index in Ints.First_Index .. Ints.Last_Index loop
         Item := Ints.Element (index);
         Assert (Item.Value_Kind = Integer_Type,
                 "Type_Utilities.To_Float_List invalid item "
                 & Integer'Image (index) & " data type is " &
                   Data_Type'Image (Item.Value_Kind));
         Values.Append (Item.Integer_Value);
      end loop;

      return Values;

   end To_Integer_List;

   --  -------------------------------------------------------------------------

   function To_Integer_Value_List (A : Integer_Array)
                                   return ML_Types.Value_Data_List is
      Data       : Value_Record (Integer_Type);
      A_List     : Value_Data_List;
   begin
      for index in A'First .. A'Last loop
         Data.Integer_Value := A (index);
         A_List.Append (Data);
      end loop;

      return A_List;
   end To_Integer_Value_List;

   --  -------------------------------------------------------------------------

   function To_Integer_Value_List_2D (A : Integer_Array)
                                      return ML_Types.Value_Data_Lists_2D is
      Data       : Value_Record (Integer_Type);
      B_List     : Value_Data_List;
      Multi_List : Value_Data_Lists_2D;
   begin
      for index in A'First .. A'Last loop
         B_List.Clear;
         Data.Integer_Value := A (index);
         B_List.Append (Data);
         Multi_List.Append (B_List);
      end loop;

      return Multi_List;
   end To_Integer_Value_List_2D;

   --  -------------------------------------------------------------------------

   function To_Multi_Value_List (A : Multi_Value_Array)
                                 return ML_Types.Value_Data_Lists_2D is
      Value    : Value_Record (Integer_Type);
      Row_List : Value_Data_Lists_2D;
      Col_List : Value_Data_List;
   begin
      for row in A'First .. A'Last loop
         Col_List.Clear;
         for col in A'Range (2) loop
            Value.Integer_Value := A (row, col);
            Col_List.Append (Value);
         end loop;
         Row_List.Append (Col_List);
      end loop;
      return Row_List;

   end To_Multi_Value_List;

   --  -------------------------------------------------------------------------

   function To_Integer_List_2D (Data : ML_Types.Value_Data_Lists_2D)
                                return ML_Types.Integer_List_2D  is
      I2_List : ML_Types.Integer_List_2D;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         I2_List.Append (To_Integer_List (Data (index)));
      end loop;

      return I2_List;

   end To_Integer_List_2D;

   --  -------------------------------------------------------------------------

   function To_Natural_List (A : Natural_Array) return NL_Types.Natural_List is
      A_List : NL_Types.Natural_List;
   begin
      for index in A'First .. A'Last loop
         A_List.Append (A (index));
      end loop;
      return A_List;

   end To_Natural_List;

   --  -------------------------------------------------------------------------

   function To_Natural_List (Numbers : ML_Types.Value_Data_List)
                             return NL_Types.Natural_List is
      Item   : Value_Record;
      Values : NL_Types.Natural_List;
   begin
      for index in Numbers.First_Index .. Numbers.Last_Index loop
         Item := Numbers.Element (index);
         Assert (Item.Value_Kind = Integer_Type,
                 "Type_Utilities.To_Natural_List invalid item "
                 & Integer'Image (index) & " data type is " &
                   Data_Type'Image (Item.Value_Kind));
         Assert (Item.Integer_Value >= 0,
                 "Type_Utilities.To_Natural_List invalid value "
                 & Integer'Image (Item.Integer_Value) & " should be >= 0.");
         Values.Append (Natural (Item.Integer_Value));
      end loop;

      return Values;

   end To_Natural_List;

   --  -------------------------------------------------------------------------

   function To_Natural_Value_List (A : Natural_Array)
                                   return ML_Types.Value_Data_Lists_2D is
      Int_Array : Integer_Array (1 .. A'Length);
   begin
      for index in A'First .. A'Last loop
         Int_Array (index) := A (index);
      end loop;
      return To_Integer_Value_List_2D (Int_Array);
   end To_Natural_Value_List;

   --  ------------------------------------------------------------------------

   function To_PL_Array (List_1D : NL_Types.Float_List; Num_Rows : Positive)
                         return Real_Float_Matrix is
      Routine_Name : constant String := "Type_Utilities.To_PL_Array ";
      Length_1D    : constant Positive := Positive (List_1D.Length);
      Num_Cols     : constant Positive := Length_1D / Num_Rows;
      End_Offset   : constant Positive := Num_Cols - 1;
      Start        : Positive := List_1D.First_Index;
      Result       : Real_Float_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
   begin
      Assert (Num_Rows * Num_Cols = Length_1D, Routine_Name & "Num_Rows" &
                Integer'Image (Num_Rows) & " is incompatible with List_1D size"
              & Integer'Image (Length_1D));

      for row in reverse 1 .. Num_Rows loop
         for col in Start .. Start + End_Offset loop
            Result (col - Start + 1, row) := Float (List_1D.Element (col));
         end loop;
         Start := Start + Num_Cols;
      end loop;

      return Result;

   end To_PL_Array;

   --  -------------------------------------------------------------------------

   function To_Value_2D_List (A : ML_Types.Value_Data_List)
                              return ML_Types.Value_Data_Lists_2D is
      Output_List : Value_Data_List;
      A2_List     : Value_Data_Lists_2D;
   begin
      A2_List.Clear;
      for index in A.First_Index .. A.Last_Index loop
         Output_List.Clear;
         Output_List.Append (A.Element (index));
         A2_List.Append (Output_List);
      end loop;

      return A2_List;

   end To_Value_2D_List;

   --  -------------------------------------------------------------------------

   function To_Value_2D_List (List_1D  : ML_Types.Value_Data_List;
                              Num_Rows : Positive)
                              return ML_Types.Value_Data_Lists_2D is
      Routine_Name : constant String := "Type_Utilities.To_Value_2D_List ";
      Length_1D    : constant Positive := Positive (List_1D.Length);
      Num_Cols     : constant Positive := Length_1D / Num_Rows;
      End_Offset   : constant Positive := Num_Cols - 1;
      Start        : Positive := List_1D.First_Index;
      Column_List  : Value_Data_List;
      List_2D      : Value_Data_Lists_2D;
   begin
      Assert (Num_Rows * Num_Cols = Length_1D, Routine_Name & "Num_Rows" &
                Integer'Image (Num_Rows) & " is incompatible with List_1D size"
              & Integer'Image (Length_1D));

      for index in 1 .. Num_Rows loop
         Column_List.Clear;
         for col in Start .. Start + End_Offset loop
            Column_List.Append (List_1D (col));
         end loop;
         List_2D.Append (Column_List);
         Start := Start + Num_Cols;
      end loop;

      return List_2D;

   end To_Value_2D_List;

   --  -------------------------------------------------------------------------

   function Transpose (Values : ML_Types.Value_Data_Lists_2D)
                       return  ML_Types.Value_Data_Lists_2D is
      use Ada.Containers;
      Num_Rows : constant Positive := Positive (Values.Length);
      Num_Cols : constant Count_Type := Values.Element (1).Length;
      In_Row   : Value_Data_List;
      Out_Row  : Value_Data_List;
      Result   : Value_Data_Lists_2D;
   begin
      Result.Set_Length (Num_Cols);
      for row in 1 .. Num_Rows loop
         In_Row := Values.Element (row);
         for index in In_Row.First_Index ..  In_Row.Last_Index loop
            Out_Row := Result.Element (index);
            Out_Row.Append (In_Row.Element (index));
            Result.Replace_Element (index, Out_Row);
         end loop;
      end loop;

      return Result;

   end Transpose;

   --  -------------------------------------------------------------------------

end Type_Utilities;
