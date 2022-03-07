
--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;

package body IL_Types is

   Precision : constant Float := 10.0 ** (-6);

   --  ------------------------------------------------------------------------

   function "-" (L, R : Float_Package.Vector) return Float_Package.Vector is
      Result : Float_Package.Vector;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) - R.Element (index));
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "abs" (aVector : Float_Package.Vector) return Float_Package.Vector is
      Result : Float_Package.Vector;
   begin
      for index in aVector.First_Index .. aVector.Last_Index loop
         Result.Append (abs (aVector.Element (index)));
      end loop;

      return Result;

   end "abs";

   --  ----------------------------------------------------------------------------

   procedure Check_Length (Routine_Name : String; L : Float_List;
                           R            : IL_Types.Value_Data_List) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " R length" & Count_Type'Image (R.Length) &
                " should be the same as L length" &
                Count_Type'Image (L.Length));
   end Check_Length;

   --  ----------------------------------------------------------------------------

   procedure Check_Length
     (Routine_Name : String; L : Value_Data_Lists_2D; R: Float_List) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " R length" & Count_Type'Image (R.Length) &
                " should be the same as L length" &
                Count_Type'Image (L.Length));
   end Check_Length;

   --  ----------------------------------------------------------------------------

   procedure Check_Length
     (Routine_Name : String; L: Float_List; R : Value_Data_Lists_2D) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " R length" & Count_Type'Image (R.Length) &
                " should be the same as L length" &
                Count_Type'Image (L.Length));
   end Check_Length;

   --  ----------------------------------------------------------------------------

   function Dot (L, R : Float_Package.Vector) return Float is
      Result : Float := 0.0;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result := Result + L.Element (index) * R.Element (index);
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function Dot (L : Float_List; R : Value_Data_Lists_2D) return Float is
      R_List : Value_Data_List;
      Result : Float := 0.0;
   begin
      Check_Length ("Dot", L, R);
      for index in R.First_Index .. R.Last_Index loop
         R_List := R.Element (index);
         for index_2 in R.First_Index .. R.Last_Index loop
            case R_List.Element (1).Value_Kind is
               when Float_Type =>
                  Result := Result + L.Element (index_2) *
                    R_List.Element (index_2).Float_Value;
               when Integer_Type =>
                  Result := Result + L.Element (index_2) *
                    Float (R_List.Element (index_2).Integer_Value);
               when Boolean_Type | UB_String_Type => null;
            end case;
         end loop;
      end loop;

      return Result;

   end Dot;

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
      Assert (R.Length = L.Length, "IL_Types." & Routine_Name &
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
                R.Element (1).Length = L.Element (1).Length, "IL_Types."
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

end IL_Types;