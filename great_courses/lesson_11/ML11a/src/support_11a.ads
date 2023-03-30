
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_11A is

   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float;
   function Cluster_Means
     (Data : Real_Float_Matrix; K : Positive; Curr_Loss : out Float;
      Test : Boolean := False) return Real_Float_Matrix;
   function Compute_Ans
     (Labels         : Integer_Matrix; Center_IDs : Integer_Array;
      Cluster_Labels : Integer_Array; Num_Clusters : Positive)
      return Real_Float_List;
   function Compute_Cluster_Labels
     (Labels       : Integer_Matrix; Center_IDs : Integer_Array;
      Num_Clusters : Positive) return Integer_Array;
   function Compute_Labelled
     (Train_Y, Test_Y : Integer_Matrix; IDs : Integer_Array) return Float;
   function Get_Cluster (Data               : Real_Float_Matrix;
                         Labels, Labels_IDs : Integer_Array; Index : Natural)
                         return Real_Float_Matrix;
   function Select_Items (Data  : Integer_Matrix;  Center_IDs : Integer_Array;
                          Index : Natural) return ML_Types.Integer_List;

end Support_11A;
