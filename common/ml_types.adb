
--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;

package body ML_Types is

   Precision : constant Float := 10.0 ** (-6);

   --  ------------------------------------------------------------------------

   function "=" (L, R : Integer_List) return Boolean is
      Result : Boolean := True;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result := Result and (L.Element (index) = R.Element (index));
      end loop;

      return Result;

   end "=";

   --  ----------------------------------------------------------------------------

   function "<" (L, R : Value_Record) return Boolean is
      Result : Boolean := L.Value_Kind = R.Value_Kind;
   begin
      if Result then
         case L.Value_Kind is
            when Boolean_Type =>
               Result := L.Boolean_Value /= R.Boolean_Value;
            when Integer_Type =>
               Result := L.Integer_Value < R.Integer_Value;
            when Float_Type =>
               Result := L.Float_Value < R.Float_Value;
            when UB_String_Type =>
               Result := L.UB_String_Value < R.UB_String_Value;
         end case;
      end if;

      return Result;
   end "<";

   --  ------------------------------------------------------------------------

   function "<=" (L, R : Value_Record) return Boolean is
      Result : Boolean := L.Value_Kind = R.Value_Kind;
   begin
      if Result then
         case L.Value_Kind is
            when Boolean_Type =>
               Result := L.Boolean_Value = R.Boolean_Value;
            when Integer_Type =>
               Result := L.Integer_Value <= R.Integer_Value;
            when Float_Type =>
               Result := L.Float_Value <= R.Float_Value;
            when UB_String_Type =>
               Result := L.UB_String_Value <= R.UB_String_Value;
         end case;
      end if;

      return Result;
   end "<=";

   --  ------------------------------------------------------------------------

   function "=" (L, R : Value_Record) return Value_Record is
      Result : Value_Record (Boolean_Type);
   begin
      case L.Value_Kind is
         when Boolean_Type =>
            Result.Boolean_Value := L.Boolean_Value = R.Boolean_Value;
         when Integer_Type =>
            Result.Boolean_Value := L.Integer_Value = R.Integer_Value;
         when Float_Type =>
            Result.Boolean_Value :=
              L.Float_Value >= R.Float_Value - Precision and then
              L.Float_Value <= R.Float_Value + Precision;
         when UB_String_Type =>
            Result.Boolean_Value := L.UB_String_Value = R.UB_String_Value;
      end case;

      return Result;
   end "=";

   --  ------------------------------------------------------------------------

   function "-" (L, R : Value_Record) return Value_Record is
      Result : Value_Record (L.Value_Kind);
   begin
      case L.Value_Kind is
         when Integer_Type =>
            Result.Integer_Value := L.Integer_Value - R.Integer_Value;
         when Float_Type =>
            Result.Float_Value := L.Float_Value - R.Float_Value;
         when Boolean_Type | UB_String_Type => null;
      end case;

      return Result;
   end "-";

   --  ------------------------------------------------------------------------

   function "*" (L, R : Value_Record) return Value_Record is
      Result : Value_Record (L.Value_Kind);
   begin
      case L.Value_Kind is
         when Integer_Type =>
            Result.Integer_Value := L.Integer_Value * R.Integer_Value;
         when Float_Type =>
            Result.Float_Value := L.Float_Value * R.Float_Value;
         when Boolean_Type | UB_String_Type => null;
      end case;

      return Result;

   end "*";

   --  ------------------------------------------------------------------------

   function "abs" (Value : Value_Record) return Value_Record is
      Result : Value_Record (Value.Value_Kind);
   begin
      case Value.Value_Kind is
         when Integer_Type =>
            Result.Integer_Value := abs (Value.Integer_Value);
         when Float_Type =>
            Result.Float_Value := abs (Value.Float_Value);
         when Boolean_Type | UB_String_Type => null;
      end case;

      return Result;

   end "abs";

   --  ------------------------------------------------------------------------

   function "+" (L, R : Value_Record) return Value_Record is
      Result : Value_Record (L.Value_Kind);
   begin
      case L.Value_Kind is
         when Integer_Type =>
            Result.Integer_Value := L.Integer_Value + R.Integer_Value;
         when Float_Type =>
            Result.Float_Value := L.Float_Value + R.Float_Value;
         when Boolean_Type | UB_String_Type => null;
      end case;

      return Result;
   end "+";

   --  ------------------------------------------------------------------------

   function "=" (L, R : Value_Data_List) return Value_Data_List is
      Result : Value_Data_List;
   begin
      Check_Length ("1D =", L, R);
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) = R.Element (index));
      end loop;

      return Result;

   end "=";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Value_Data_List) return Value_Data_List is
      Result : Value_Data_List;
   begin
      Check_Length ("-", L, R);
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) - R.Element (index));
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "abs" (aVector : Value_Data_List) return Value_Data_List is
      Result : Value_Data_List;
   begin
      for index in aVector.First_Index .. aVector.Last_Index loop
         Result.Append (abs (aVector.Element (index)));
      end loop;

      return Result;

   end "abs";

   --  ----------------------------------------------------------------------------

   function Dot (L, R : Value_Data_List) return Value_Record is
      Result : Value_Record (L.First_Element.Value_Kind);
   begin
      Check_Length ("Dot", L, R);
      for index in L.First_Index .. L.Last_Index loop
         Result := Result + L.Element (index) * R.Element (index);
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function "=" (L, R : Value_Data_Lists_2D) return Value_Data_Lists_2D is
      Result : Value_Data_Lists_2D;
   begin
      Check_Lengths ("2D =", L, R);
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) = R.Element (index));
      end loop;

      return Result;

   end "=";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Value_Data_Lists_2D) return Value_Data_Lists_2D is
      Result : Value_Data_Lists_2D;
   begin
      Check_Lengths ("-", L, R);
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) - R.Element (index));
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "abs" (aVector : Value_Data_Lists_2D) return Value_Data_Lists_2D is
      Result : Value_Data_Lists_2D;
   begin
      for index in aVector.First_Index .. aVector.Last_Index loop
         Result.Append (abs (aVector.Element (index)));
      end loop;

      return Result;

   end "abs";

   --  ----------------------------------------------------------------------------

   procedure Check_Length (Routine_Name : String; L, R : Value_Data_List) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, "ML_Types." & Routine_Name &
                " right length" & Count_Type'Image (R.Length) &
                " should be the same as left length" &
                Count_Type'Image (L.Length));
   end Check_Length;

   --  ----------------------------------------------------------------------------

   procedure Check_Lengths (Routine_Name : String;
                           L, R         : Value_Data_Lists_2D) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length and then
                R.Element (1).Length = L.Element (1).Length, "ML_Types."
                & Routine_Name &
                " right lengths (" & Count_Type'Image (R.Length) & ", " &
                Count_Type'Image (R.Element (1).Length) &
                ") should be the same as left lengths (" &
                Count_Type'Image (L.Length) & ", " &
                Count_Type'Image (L.Element (1).Length) & ").");
   end Check_Lengths;

   --  ----------------------------------------------------------------------------

   function Dot (L, R : Value_Data_Lists_2D) return Value_Record is
      L_Data_List : Value_Data_List := L.First_Element;
      R_Data_List : Value_Data_List;
      Result      : Value_Record (L_Data_List.First_Element.Value_Kind);
   begin
      Check_Lengths ("Dot", L, R);
      for index in L.First_Index .. L.Last_Index loop
         L_Data_List := L.Element (index);
         R_Data_List := R.Element (index);
         for index in L_Data_List.First_Index .. L_Data_List.Last_Index loop
            Result := Result +
              L_Data_List.Element (index) * R_Data_List.Element (index);
         end loop;
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function Max (Data : Integer_List) return Integer is
      Result : Integer := Data.First_Element;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         if Data (Integer (index)) > Result then
            Result := Data (Integer (index));
         end if;
      end loop;

      return Result;

   end Max;

   --  ------------------------------------------------------------------------

   function Min (Data : Integer_List) return Integer is
      Result : Integer := Data.First_Element;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         if Data (Integer (index)) < Result then
            Result := Data (Integer (index));
         end if;
      end loop;

      return Result;

   end Min;

   --  ------------------------------------------------------------------------

   function Slice (aList : Integer_List_2D; First, Last : Positive)
                   return Integer_List_2D is
      Result    : Integer_List_2D;
      List_Row  : Integer_List;
      Slice_Row : Integer_List;
   begin
      for row in First .. Last loop
         List_Row := aList.Element (row);
         Slice_Row.Clear;
         for col in Integer (List_Row.First_Index) .. Integer (List_Row.Last_Index) loop
            Slice_Row.Append (List_Row.Element (col));
         end loop;
         Result.Append (Slice_Row);
      end loop;

      return Result;

   end Slice;

   --  ------------------------------------------------------------------------

   function Sum (Data : Integer_List) return Integer is
      Result : Integer := 0;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         Result := Result + Data (Integer (index));
      end loop;

      return Result;

   end Sum;

   --  ------------------------------------------------------------------------

   function Transpose (Values : Integer_List_2D) return  Integer_List_2D is
      use Ada.Containers;
      Num_Rows : constant Positive := Positive (Values.Length);
      Num_Cols : constant Count_Type := Values.Element (1).Length;
      In_Row   : Integer_List;
      Out_Row  : Integer_List;
      Result   : Integer_List_2D;
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

end ML_Types;
