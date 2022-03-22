--  Based on scikit-learn/sklearn/model_selection/_split.py

with NL_Types; use NL_Types;

package Data_Splitter2 is

   type Base_Shuffle_Data is private;

   procedure Init_Base_Shuffle_Split
     (Self : in out Base_Shuffle_Data; Num_Splits : Natural;
      Train_Size, Test_Size, Default_Test_Size : Natural);
   procedure Train_Test_Split
     (X : Float_List_2D; Y : Integer_List_2D; Train_Size, Test_Size : Natural;
      Train_X : out Float_List_2D; Train_Y : out Integer_List_2D;
      Test_X  : out Float_List_2D; Test_Y : out Integer_List_2D);

private

   type Base_Shuffle_Data is record
      Num_Splits        : Natural := 0;
      Train_Size        : Natural := 0;
      Test_Size         : Natural := 0;
      Default_Test_Size : Natural := 0;
      X_Train_Set       : Float_List_2D;
      Y_Train_Set       : Integer_List;
      X_Test_Set        : Float_List_2D;
      Y_Test_Set        : Integer_List;
   end record;

end Data_Splitter2;
