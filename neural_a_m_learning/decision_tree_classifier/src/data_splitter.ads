--  Based on scikit-learn/sklearn/model_selection/_split.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Data_Splitter is

--     type Base_Shuffle_Data (Train_Size, Test_Size, Num_Features : Positive) is
--     private;
--
--     procedure Init_Base_Shuffle_Split
--       (Self              : in out Base_Shuffle_Data; Num_Splits : Natural;
--        Default_Test_Size : Natural);
   procedure Train_Test_Split
     (X  : Float_Matrix; Y : Integer_Array;
      Train_Size, Test_Size : Natural;
      Train_X : out Float_Matrix; Train_Y : out Integer_Array;
      Test_X  : out Float_Matrix; Test_Y : out Integer_Array);

private

   type Base_Shuffle_Data (Train_Size, Test_Size, Num_Features : Positive) is
      record
         Num_Splits        : Natural := 0;
         Default_Test_Size : Natural := 0;
         X_Train_Set       : Float_Matrix (1 .. Train_Size, 1 .. Num_Features);
         Y_Train_Set       : Integer_Array (1 .. Test_Size);
         X_Test_Set        : Float_Matrix (1 .. Train_Size, 1 .. Num_Features);
         Y_Test_Set        : Integer_Array (1 .. Test_Size);
      end record;

end Data_Splitter;
