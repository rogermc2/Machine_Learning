
package body NL_Arrays_And_Matrices is

   function To_Integer_Array (List : Integer_List) return Integer_Array is
      Result : Integer_Array (1 .. Positive (List.Length));
   begin
      return Result;

   end To_Integer_Array;

   --  ------------------------------------------------------------------------

   function To_Float_Array (List : Float_List) return Float_Array is
      Result : Float_Array (1 .. Positive (List.Length));
   begin
      return Result;

   end To_Float_Array;

   --  ------------------------------------------------------------------------

   function To_Float_Matrix (List : Float_List_2D) return Float_Matrix is
      Result : Float_Matrix (1 .. Positive (List.Length),
                             1 .. Positive (List (1).Length));
   begin
      return Result;

   end To_Float_Matrix;

   --  ------------------------------------------------------------------------

end NL_Arrays_And_Matrices;
