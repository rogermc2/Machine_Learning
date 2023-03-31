
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_11A is

   function Assign_Data_To_Clusters
     (Data, Cluster_Centres : Real_Float_Matrix; Centre_Ids : out Integer_Array)
      return Float;
   function Cluster_Means
     (Data : Real_Float_Matrix; Num_Clusters : Positive; Curr_Loss : out Float;
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
   procedure Get_Best_Centres
     (Data         : Real_Float_Matrix; Num_Clusters : Positive;
      Best_Centres : in out Real_Float_Matrix; Best_Loss : in out Float);
   function Get_Cluster (Data               : Real_Float_Matrix;
--                           Cluster_Labels,
                         Center_IDs         : Integer_Array;
                         Cluster_ID         : Natural)
                         return Real_Float_Matrix;
   function Select_Items (Data  : Integer_Matrix;  Center_IDs : Integer_Array;
                          Index : Natural) return ML_Types.Integer_List;

end Support_11A;
