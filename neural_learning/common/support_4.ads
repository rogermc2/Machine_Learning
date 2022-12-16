
with Load_Dataset;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_4 is

   type Base_State (Num_Train, Num_Test, Num_Features : Positive) is record
      Train_X : Real_Float_Matrix (1 .. Num_Train, 1 .. Num_Features);
      Train_Y : Integer_Matrix (1 .. Num_Train, 1 .. 1);
      Test_X  : Real_Float_Matrix (1 .. Num_Test, 1 .. Num_Features);
      Test_Y  : Integer_Matrix (1 .. Num_Test, 1 .. 1);
   end record;

   function Get_State
     (Dataset               : Load_Dataset.Digits_Data_Record;
      Train_Size, Test_Size : Positive;
      Shuffle               : Boolean := True) return Base_State;
   function Get_State
     (Dataset_Name : String; Train_Size, Test_Size : Positive;
      Shuffle      : Boolean := True) return Base_State;

end Support_4;
