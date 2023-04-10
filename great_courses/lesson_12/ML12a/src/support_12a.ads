
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_12A is

   function Get_Plot_Data
     (Data         : Real_Float_Matrix; Cluster_Labels : Integer_Array;
      Center_IDs   : Integer_Array; Cluster_ID : Natural;
      Col_1, Col_2 : Positive) return Real_Float_Matrix ;
   function Select_Items (Data  : Integer_Matrix;  Center_IDs : Integer_Array;
                          Index : Natural) return ML_Types.Integer_List;
   function Load_Data (File_Name : String) return ML_Types.Unbounded_List;

end Support_12A;
