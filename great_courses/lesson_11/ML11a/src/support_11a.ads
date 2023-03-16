
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_11A is

   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Matrix) return Float;
   function Cluster_Means (Data : Real_Float_Matrix; K : Positive;
                           Curr_Loss : out Float) return Real_Float_Matrix;

end Support_11A;
