
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_9AUX_2 is

   type Data_Record (Num_Items, Num_Features : Positive) is record
      Features : Real_Float_Matrix (1 .. Num_Items, 1 .. Num_Features);
      Labels   : Integer_Matrix (1 .. Num_Items, 1 .. 1);
   end record;

   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Matrix) return Float;
   function Init_MS (Length : Positive) return Integer_Array;
   function Mini_Data (Data : CSV_Data_Loader.Base_Split_State;
                       MS   : Positive) return Data_Record;

end Support_9AUX_2;
