
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Shuffler is

   procedure Shuffle (A : in out Integer_Array);
   procedure Shuffle (A : in out Real_Float_Matrix);
   procedure Shuffle (A : in out Real_Float_Matrix; B : in out Binary_Matrix);
   procedure Shuffle (A : in out Real_Float_Matrix; B : in out Integer_Array);
   procedure Shuffle (A : in out Real_Float_Matrix; B : in out Integer_Matrix);

end Shuffler;
