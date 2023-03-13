
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_11A is

   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float;
   function Compute_Means (Data       : Real_Float_Matrix;
                           Centre_Ids : Integer_Array; K : Positive)
                           return Real_Float_Matrix;

end Support_11A;
