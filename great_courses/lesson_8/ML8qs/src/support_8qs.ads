
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_8QS is

   type Data_Record (Num_Items, Num_Features : Positive) is record
      Features : Real_Float_Matrix (1 .. Num_Items, 1 .. Num_Features);
      Labels   : Integer_Array (1 .. Num_Items);
   end record;

   function Get_Data (File_Name : String; Num_Samples : Positive)
                      return Data_Record;
   function Get_Mins (M1, M2 : Real_Float_Matrix)  return Real_Float_List;
   function Test_Score (Predictions : Real_Float_Vector;
                        Labels      : Integer_Array) return Natural;

end Support_8QS;
