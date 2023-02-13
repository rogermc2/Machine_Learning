
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_Iris is

   type Data_Record is record
      Features : Real_Float_List;
      Labels   : ML_Types.Integer_List;
   end record;

   function Categorize (Labels : ML_Types.Unbounded_List) return Integer_Array;
   function Load_Data (File_Name : String) return Data_Record;
   function Test_Score (Predictions : Real_Float_Vector;
                        Labels      : Integer_Array) return Natural;

end Support_Iris;
