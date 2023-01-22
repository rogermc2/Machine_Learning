
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package CSV_Data_Loader is

   type Data_Kind is (Digits_Data, Iris_Data, Diabetes_Data);

   type Base_Split_State (Num_Train, Num_Test, Num_Features : Positive)
   is record
      Train_X : Real_Float_Matrix (1 .. Num_Train, 1 .. Num_Features);
      Train_Y : Binary_Matrix (1 .. Num_Train, 1 .. 10);
      Test_X  : Real_Float_Matrix (1 .. Num_Test, 1 .. Num_Features);
      Test_Y  : Real_Float_Vector (1 .. Num_Test);
   end record;

   function Get_Split_State
     (Dataset_Name : String; Data_Type : Data_Kind;
      Train_Size   : Positive; Test_Size : Positive;
      Shuffle      : Boolean := True; Reload : Boolean := False)
      return Base_Split_State;

end CSV_Data_Loader;
