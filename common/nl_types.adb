
--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;

package body NL_Types is

   --  ------------------------------------------------------------------------

   procedure Check_Length
     (Routine_Name : String; L, R : Float_List_2D) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " R length" & Count_Type'Image (R.Length) &
                " should be the same as L length" &
                Count_Type'Image (L.Length));
      Assert (R (1).Length = L (1).Length, Routine_Name &
                " R (1) length" & Count_Type'Image (R (1).Length) &
                " should be the same as L (1) length" &
                Count_Type'Image (L (1).Length));
   end Check_Length;

   --  ----------------------------------------------------------------------------

   function ">" (L, R : Float_List) return Boolean is
      Result : Boolean := True;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result := Result and L.Element (index) > R.Element (index);
      end loop;

      return Result;

   end ">";

   --  ----------------------------------------------------------------------------

   function "=" (L, R : Float_List) return Boolean is
      Result : Boolean := True;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result := Result and L.Element (index) = R.Element (index);
      end loop;

      return Result;

   end "=";

   --  ----------------------------------------------------------------------------

   function "+" (L, R : Float_List) return Float_List is
      Result : Float_List;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) + R.Element (index));
      end loop;

      return Result;

   end "+";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Float_List) return Float_List is
      Result : Float_List;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) - R.Element (index));
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "*" (L : Float; R : Float_List) return Float_List is
      Result : Float_List;
   begin
      for index in R.First_Index .. R.Last_Index loop
         Result.Append (L * R.Element (index));
      end loop;

      return Result;

   end "*";

   --  -------------------------------------------------------------------------

   function "**" (L : Float_List; P : Integer) return Float_List is
      Result : Float_List;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) ** P);
      end loop;

      return Result;

   end "**";

   --  ----------------------------------------------------------------------------

   function "*" (L, R : Float_List) return Float_List is
      Result : Float_List;
   begin
      for index in R.First_Index .. R.Last_Index loop
         Result.Append (L.Element (index) * R.Element (index));
      end loop;

      return Result;

   end "*";

   --  ----------------------------------------------------------------------------

   function "/" (L : Float_List; R : Float) return Float_Package.Vector is
      Result : Float_List;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) / R);
      end loop;

      return Result;

   end "/";

   --  ----------------------------------------------------------------------------

   function "abs" (aVector : Float_List) return Float_List is
      Result : Float_List;
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

   function ">" (L, R : Float_List_2D) return Boolean is
      Result : Boolean := True;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result := Result and L.Element (index) > R.Element (index);
      end loop;

      return Result;

   end ">";

   --  -------------------------------------------------------------------------

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
      Recip_R : constant Float := 1.0 / R;
      List_1D : Float_List;
      Result  : Float_List_2D;
   begin
      for row in L.First_Index .. L.Last_Index loop
         List_1D := L (row);
         for col in List_1D.First_Index .. List_1D.Last_Index loop
            List_1D (col) := Recip_R * List_1D (col);
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
      Check_Length ("Float_List_2D + ", L, R);
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
      L_Row   : Float_List;
      R_Col   : Float_List;
      LR_Row  : Float_List;
      LR      : Float;
      Product : Float_List_2D;
   begin
      for index in L.First_Index .. L.Last_Index loop
         L_Row := L (index);
         for col_index in RT.First_Index .. RT.Last_Index loop
            R_Col := RT (col_index);
            LR := 0.0;
            LR_Row.Clear;
            for lr_index in L_Row.First_Index .. L_Row.Last_Index loop
               LR := LR + L_Row (lr_index) * R_Col (lr_index);
            end loop;
            LR_Row.Append (LR);
         end loop;
         Product.Append (LR_Row);
      end loop;

      return Product;

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

   procedure Check_Lengths (Routine_Name : String; L : ML_Types.Integer_List;
                            R            : Float_List) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " R length" & Count_Type'Image (R.Length) &
                " should be the same as L length" &
                Count_Type'Image (L.Length));
   end Check_Lengths;

   --  ----------------------------------------------------------------------------
--
--     function Transpose (Values : ML_Types.Integer_List_2D)
--                         return  ML_Types.Integer_List_2D is
--        use Ada.Containers;
--        use ML_Types;
--        Num_Rows : constant Positive := Positive (Values.Length);
--        Num_Cols : constant Count_Type := Values.Element (1).Length;
--        In_Row   : Integer_List;
--        Out_Row  : Integer_List;
--        Result   : Integer_List_2D;
--     begin
--        Result.Set_Length (Num_Cols);
--        for row in 1 .. Num_Rows loop
--           In_Row := Values.Element (row);
--           for index in In_Row.First_Index ..  In_Row.Last_Index loop
--              Out_Row := Result.Element (index);
--              Out_Row.Append (In_Row.Element (index));
--              Result.Replace_Element (index, Out_Row);
--           end loop;
--        end loop;
--
--        return Result;
--
--     end Transpose;

   --  -------------------------------------------------------------------------

end NL_Types;
