
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_11A is

   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float;
   function Cluster_Means
     (Data : Real_Float_Matrix; K : Positive; Curr_Loss : out Float;
      Test : Boolean := False) return Real_Float_Matrix;
   function Stat_Mode (A : Integer_Array) return Integer;

end Support_11A;
