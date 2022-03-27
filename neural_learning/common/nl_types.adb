
--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;

package body NL_Types is

   Precision : constant Float := 10.0 ** (-6);

   --  ------------------------------------------------------------------------

   function "+" (L, R : Float_Package.Vector) return Float_Package.Vector is
      Result : Float_Package.Vector;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) + R.Element (index));
      end loop;

      return Result;

   end "+";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Float_Package.Vector) return Float_Package.Vector is
      Result : Float_Package.Vector;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) - R.Element (index));
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "*" (L : Float; R : Float_Package.Vector)
                 return Float_Package.Vector is
      Result : Float_Package.Vector;
   begin
      for index in R.First_Index .. R.Last_Index loop
         Result.Append (L * R.Element (index));
      end loop;

      return Result;

   end "*";

   --  -------------------------------------------------------------------------

   function "**" (L : Float_Package.Vector; P : Integer)
                  return Float_Package.Vector is
      Result : Float_Package.Vector;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) ** P);
      end loop;

      return Result;

   end "**";

   --  ----------------------------------------------------------------------------

   function "*" (L, R : Float_Package.Vector) return Float_Package.Vector is
      Result : Float_Package.Vector;
   begin
      for index in R.First_Index .. R.Last_Index loop
         Result.Append (L.Element (index) * R.Element (index));
      end loop;

      return Result;

   end "*";

   --  ----------------------------------------------------------------------------

   function "/" (L : Float_Package.Vector; R : Float)
                 return Float_Package.Vector is
      Result : Float_Package.Vector;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) / R);
      end loop;

      return Result;

   end "/";

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

   procedure Check_Lengths (Routine_Name : String; L, R : Float_List) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " right length" & Count_Type'Image (R.Length) &
                " should be the same as left length" &
                Count_Type'Image (L.Length));
   end Check_Lengths;

   --  ----------------------------------------------------------------------------

   procedure Check_Length (Routine_Name : String; L : Float_List;
                           R            : Float_List_2D) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " R length" & Count_Type'Image (R.Length) &
                " should be the same as L length" &
                Count_Type'Image (L.Length));
   end Check_Length;

   --  ----------------------------------------------------------------------------

   function "*" (L : Float; R : Float_List_2D) return Float_List_2D is
      List_1D : Float_List;
      Result  : Float_List_2D;
   begin
      for row in R.First_Index .. R.Last_Index loop
         List_1D := R (row);
         for col in List_1D.First_Index .. List_1D.Last_Index loop
            List_1D (col) := L * List_1D (col);
         end loop;
         Result.Append (List_1D);
      end loop;

      return Result;

   end "*";

   --  ----------------------------------------------------------------------------

   function "*" (L, R : Float_List_2D) return Float_List_2D is
      List_1D : Float_List;
      List_2D : Float_List;
      List_3D : Float_List;
      Result  : Float_List_2D;
   begin
      for row in L.First_Index .. L.Last_Index loop
         List_1D := L (row);
         List_2D := R (row);
         for col in List_1D.First_Index .. List_1D.Last_Index loop
            List_3D (col) := List_1D (col) * List_2D (col);
         end loop;
         Result.Append (List_3D);
      end loop;

      return Result;

   end "*";

   --  ----------------------------------------------------------------------------

   function "**" (L : Float_List_2D; P : Integer) return Float_List_2D is
      List_1D : Float_List;
      Result  : Float_List_2D;
   begin
      for row in L.First_Index .. L.Last_Index loop
         List_1D := L (row);
         for col in List_1D.First_Index .. List_1D.Last_Index loop
            List_1D (col) := List_1D (col) ** P;
         end loop;
         Result.Append (List_1D);
      end loop;

      return Result;

   end "**";

   --  ----------------------------------------------------------------------------

   function "/" (L : Float_List_2D; R : Float) return Float_List_2D is
      List_1D : Float_List;
      Result  : Float_List_2D;
   begin
      for row in L.First_Index .. L.Last_Index loop
         List_1D := L (row);
         for col in List_1D.First_Index .. List_1D.Last_Index loop
            List_1D (col) := List_1D (col) / R;
         end loop;
         Result.Append (List_1D);
      end loop;

      return Result;

   end "/";

   --  ----------------------------------------------------------------------------

   function "+" (L, R : Float_List_2D) return Float_List_2D is
      List_1D : Float_List;
      List_2D : Float_List;
      List_3D : Float_List;
      Result  : Float_List_2D;
   begin
      for row in L.First_Index .. L.Last_Index loop
         List_1D := L (row);
         List_2D := R (row);
         List_3D.Clear;
         for col in List_1D.First_Index .. List_1D.Last_Index loop
            List_3D.Append (List_1D (col) + List_2D (col));
         end loop;
         Result.Append (List_3D);
      end loop;

      return Result;

   end "+";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Float_List_2D) return Float_List_2D is
      List_1D : Float_List;
      List_2D : Float_List;
      List_3D : Float_List;
      Result  : Float_List_2D;
   begin
      for row in L.First_Index .. L.Last_Index loop
         List_1D := L (row);
         List_2D := R (row);
         List_3D.Clear;
         for col in List_1D.First_Index .. List_1D.Last_Index loop
            List_3D.Append (List_1D (col) - List_2D (col));
         end loop;
         Result.Append (List_3D);
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function Dot (L : Float_List; R : Float_List_2D) return Float_List is
      R_List : Float_List;
      Result : Float_List;
   begin
      Check_Length ("Dot", L, R);
      for index in R.First_Index .. R.Last_Index loop
         R_List := R.Element (index);
         for index_2 in R.First_Index .. R.Last_Index loop
            Result (index) := Result (index) + L.Element (index_2) *
              R_List.Element (index_2);
         end loop;
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function Dot (L, R : Float_List) return Float_List_2D is
      R_List : Float_List;
      Result : Float_List_2D;
   begin
      for index in L.First_Index .. L.Last_Index loop
         R_List.Clear;
         for index_2 in R.First_Index .. R.Last_Index loop
            R_List (index_2) := L.Element (index_2) * R.Element (index_2);
         end loop;
         Result.Append (R_List);
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function Dot (L, R : Float_List_2D) return Float_List_2D is
      RT      : constant Float_List_2D := Transpose (R);
      L_List  : Float_List;
      RT_List : Float_List;
      LR_List : Float_List;
      LR      : Float;
      Result  : Float_List_2D;
   begin
      for l_index in L.First_Index .. L.Last_Index loop
         L_List := L (l_index);
         LR := 0.0;
         LR_List.Clear;
         for r_index in RT.First_Index .. RT.Last_Index loop
           RT_List := RT (r_index);
            for index_2 in L.First_Index .. L.Last_Index loop
               LR := LR + L_List (index_2) * RT_List (index_2);
            end loop;
            LR_List.Append (LR);
         end loop;
         Result.Append (LR_List);
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function Transpose (Values : Float_List_2D) return  Float_List_2D is
      use Ada.Containers;
      Num_Rows : constant Positive := Positive (Values.Length);
      Num_Cols : constant Count_Type := Values.Element (1).Length;
      In_Row   : Float_List;
      Out_Row  : Float_List;
      Result   : Float_List_2D;
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

   procedure Check_Lengths (Routine_Name : String; L : Integer_List;
                            R            : Float_List) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " R length" & Count_Type'Image (R.Length) &
                " should be the same as L length" &
                Count_Type'Image (L.Length));
   end Check_Lengths;

   --  ----------------------------------------------------------------------------

   procedure Check_Length (Routine_Name : String; L : Float_List;
                           R            : Value_Data_List) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " R length" & Count_Type'Image (R.Length) &
                " should be the same as L length" &
                Count_Type'Image (L.Length));
   end Check_Length;

   --  ----------------------------------------------------------------------------

   procedure Check_Length
     (Routine_Name : String; L : Value_Data_Lists_2D; R : Float_List) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " R length" & Count_Type'Image (R.Length) &
                " should be the same as L length" &
                Count_Type'Image (L.Length));
   end Check_Length;

   --  ----------------------------------------------------------------------------

   procedure Check_Length
     (Routine_Name : String; L : Float_List; R : Value_Data_Lists_2D) is
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
      Assert (R.Length = L.Length, "NL_Types." & Routine_Name &
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
              R.Element (1).Length = L.Element (1).Length, "NL_Types."
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

end NL_Types;
