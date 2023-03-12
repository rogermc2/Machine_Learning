
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package CSV_Data_Loader is

   type Data_Kind is (Digits_Data, Iris_Data, Diabetes_Data, Ship_Data);

   type Base_Split_State (Num_Train, Num_Test, Num_Features : Positive;
                          Y_Categorized : Boolean)
   is record
      Train_X : Real_Float_Matrix (1 .. Num_Train, 1 .. Num_Features);
      Test_X  : Real_Float_Matrix (1 .. Num_Test, 1 .. Num_Features);
      case Y_Categorized is
         when True =>
            Cat_Train_Y : Binary_Matrix (1 .. Num_Train, 1 .. 10);
            Cat_Test_Y  : Real_Float_Vector (1 .. Num_Test);
         when False =>
            Train_Y : Integer_Matrix (1 .. Num_Train, 1 .. 1);
            Test_Y  : Integer_Matrix (1 .. Num_Test, 1 .. 1);
      end case;
   end record;

   function Get_Split_State
     (File_Name  : String; Data_Type : Data_Kind;
      Train_Size    : Positive; Test_Size : Positive;
      Y_Categorized, Shuffle : Boolean := False;
      Normalize, Reload      : Boolean := True) return Base_Split_State;

end CSV_Data_Loader;
