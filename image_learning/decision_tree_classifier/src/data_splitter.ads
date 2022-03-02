--  Based on scikit-learn/sklearn/model_selection/_split.py

with IL_Types;

package Data_Splitter is

   type Base_Shuffle_Data is private;

   procedure Init_Base_Shuffle_Split
     (Self : in out Base_Shuffle_Data; Num_Splits : Natural;
      Train_Size, Test_Size, Default_Test_Size : Natural);
   procedure Train_Test_Split
     (X, Y : IL_Types.Value_Data_Lists_2D; Train_Size, Test_Size : Natural;
      Train_X, Train_Y, Test_X, Test_Y : out IL_Types.Value_Data_Lists_2D);

private

   type Base_Shuffle_Data is record
      Num_Splits        : Natural := 0;
      Train_Size        : Natural := 0;
      Test_Size         : Natural := 0;
      Default_Test_Size : Natural := 0;
      X_Train_Set       : IL_Types.Value_Data_Lists_2D;
      Y_Train_Set       : IL_Types.Value_Data_Lists_2D;
      X_Test_Set        : IL_Types.Value_Data_Lists_2D;
      Y_Test_Set        : IL_Types.Value_Data_Lists_2D;
   end record;

end Data_Splitter;
