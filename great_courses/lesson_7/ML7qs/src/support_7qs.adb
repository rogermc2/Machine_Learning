
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;

package body Support_7Qs is

   function Target (X : Float) return Float;

   --  -------------------------------------------------------------------------

   function Fit (Data : Real_Float_Matrix) return Real_Float_Vector is
      --        Routine_Name : constant String := "Support_7A.Load_Data ";
      Result            : Real_Float_Vector (Data'Range);
   begin
      for index in Data'Range loop
         Result (index) := Target (Data (index, 1));
      end loop;

      return Result;

   end Fit;

   --  -------------------------------------------------------------------------

   function Load_Data (Num_Samples : Positive) return Real_Float_Matrix is
      Data  : Real_Float_Matrix (1 .. Num_Samples, 1 .. 1);
   begin
      for index in Data'Range loop
         Data (index, 1) := 10.0 * Maths.Random_Float;
      end loop;

      return Data;

   end Load_Data;

   --  -------------------------------------------------------------------------

   function Load_Sorted_Data (Num_Samples : Positive)
                              return Real_Float_Matrix is
      Data_Array : Float_Array (1 .. Num_Samples);
      Data       : Real_Float_Matrix (1 .. Num_Samples, 1 .. 1);
   begin
      for index in Data_Array'Range loop
         Data_Array (index) := 10.0 * Maths.Random_Float;
      end loop;

      Float_Array_Sort (Data_Array);

      for index in Data'Range loop
         Data (index, 1) := Data_Array (index);
      end loop;

      return Data;

   end Load_Sorted_Data;

   --  -------------------------------------------------------------------------

   function Target (X : Float) return Float is
   begin
      return abs (X - 4.0) + 4.0;
   end Target;

   --  -------------------------------------------------------------------------

end Support_7Qs;
