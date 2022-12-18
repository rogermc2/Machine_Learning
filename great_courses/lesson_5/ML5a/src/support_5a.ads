
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_5A is

   function Get_Part
     (Image : Unsigned_8_Array_3D; First_Row, Last_Row : Positive;
       First_Column, Last_Column : Positive) return Unsigned_8_Array_3D;
   function Get_Picture (File_Name : String) return Unsigned_8_Array_3D;
   function Loss (Data   : Integer_Matrix; Weights  : Real_Float_Vector;
                  Labels : Integer_Array) return Float;
   function Set_All_Data (Yes_List, No_List : Integer_Matrix)
                          return Integer_Matrix;
   function To_2D (From : Unsigned_8_Array_3D) return Integer_Matrix;

end Support_5A;
