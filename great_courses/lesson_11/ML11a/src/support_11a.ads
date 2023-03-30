
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_11A is

   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float;
   function Cluster_Means
     (Data : Real_Float_Matrix; K : Positive; Curr_Loss : out Float;
      Test : Boolean := False) return Real_Float_Matrix;
   function Compute_Cluster_Labels
     (Labels       : Integer_Matrix; Center_IDs : Integer_Array;
      Num_Clusters : Positive) return Integer_Array;

end Support_11A;
