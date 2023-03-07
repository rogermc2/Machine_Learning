
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_9QS is

   type Data_Record (Num_Items, Num_Features : Positive) is record
      Features : Real_Float_Matrix (1 .. Num_Items, 1 .. Num_Features);
      Labels   : Integer_Array (1 .. Num_Items);
   end record;

   function Get_Data (File_Name : String) return Data_Record;
   function Test_Score (Predictions : Real_Float_Vector;
                        Labels      : Integer_Array) return Float;

end Support_9QS;
