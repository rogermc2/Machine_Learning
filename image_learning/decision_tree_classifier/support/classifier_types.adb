
with Ada.Assertions; use Ada.Assertions;

package body Classifier_Types is

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

   procedure Check_Length (Routine_Name : String; L : Float_List;
                           R            : IL_Types.Value_Data_Lists_2D) is
      use Ada.Containers;
   begin
      Assert (R.Length = L.Length, Routine_Name &
                " R length" & Count_Type'Image (R.Length) &
                " should be the same as L length" &
                Count_Type'Image (L.Length));
   end Check_Length;

   --  ----------------------------------------------------------------------------

   procedure Check_Length
     (Routine_Name : String; L : IL_Types.Value_Data_Lists_2D;
      R            : Float_List) is
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

   function Dot (L : Classifier_Types.Float_List; R : IL_Types.Value_Data_Lists_2D)
                 return Float is
      use IL_Types;
      R_List : Value_Data_List;
      Result : Float := 0.0;
   begin
      Check_Length ("Classifier_Types.Dot", L, R);
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

end Classifier_Types;
