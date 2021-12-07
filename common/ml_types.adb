
package body ML_Types is

    Precision : constant Float := 10.0 ** (-6);

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
                  L.Float_Value >= R.Float_Value - Precision and
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
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) = R.Element (index));
      end loop;

      return Result;

   end "=";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Value_Data_List) return Value_Data_List is
      Result : Value_Data_List;
   begin
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
      for index in L.First_Index .. L.Last_Index loop
         Result := Result + L.Element (index) * R.Element (index);
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function "=" (L, R : Value_Data_Lists_2D) return Value_Data_Lists_2D is
      Result : Value_Data_Lists_2D;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) = R.Element (index));
      end loop;

      return Result;

   end "=";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Value_Data_Lists_2D) return Value_Data_Lists_2D is
      Result : Value_Data_Lists_2D;
   begin
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

   function Dot (L, R : Value_Data_Lists_2D) return Value_Record is
      L_Data_List : Value_Data_List := L.First_Element;
      R_Data_List : Value_Data_List;
      Result      : Value_Record (L_Data_List.First_Element.Value_Kind);
   begin
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

end ML_Types;
