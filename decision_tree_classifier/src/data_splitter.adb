
package body Data_Splitter is

   type Base_Shuffle_Split is record
      Num_Splits        : Natural := 0;
      Test_Size         : Float := 0.0;
      Default_Test_Size : Float := 0.0;
      Train_Size        : Float := 0.0;
      Train_Set         : ML_Types.List_Of_Value_Data_Lists;
      Test_Set          : ML_Types.List_Of_Value_Data_Lists;
   end record;

   --  -------------------------------------------------------------------------------
   --  Shuffle_Split generates indices to split data into training and test set
   procedure Shuffle_Split (Self : in out Base_Shuffle_Split;
                            X, Y : ML_Types.List_Of_Value_Data_Lists) is
   begin
      null;
   end Shuffle_Split;

   --  -------------------------------------------------------------------------------

   procedure Train_Test_Split (X, Y : ML_Types.List_Of_Value_Data_Lists) is
   begin
      null;
   end Train_Test_Split;

end Data_Splitter;
