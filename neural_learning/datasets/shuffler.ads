
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Shuffler is

   procedure Column_Shuffle (A : in out Real_Float_Matrix);
   procedure Column_Shuffle (A : in out Real_Float_Matrix;
                             B : in out Integer_Array);
   procedure Shuffle (A : in out Integer_Array);
   procedure Shuffle (A : in out Real_Float_Matrix);
   procedure Shuffle (A : in out Real_Float_Matrix; B : in out Binary_Matrix);
   procedure Shuffle (A : in out Real_Float_Matrix; B : in out Integer_Array);
   procedure Shuffle (A : in out Real_Float_Matrix; B : in out Integer_Matrix);
   procedure Shuffle (A, B : in out Real_Float_Matrix);

end Shuffler;
