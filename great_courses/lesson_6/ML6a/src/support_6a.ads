
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_6A is

   function Get_Pixels
     (Image                     : Unsigned_8_Array_3D; First_Row, Last_Row : Positive;
      First_Column, Last_Column : Positive; D3 : Positive := 3)
      return Unsigned_8_Array_3D;
   function Get_Picture (File_Name : String) return Unsigned_8_Array_3D;
   function Set_All_Data (Yes_List, No_List : Integer_Matrix)
                          return Integer_Matrix;
   function To_2D (From : Unsigned_8_Array_3D) return Integer_Matrix;
   function To_Boolean (From : Real_Float_Vector) return Boolean_Array;
   function To_Picture (Flat_Data : Integer_Matrix;
                        Height, Width : Positive; Weights : Real_Float_Vector)
                        return Unsigned_8_Array_3D;

end Support_6A;
