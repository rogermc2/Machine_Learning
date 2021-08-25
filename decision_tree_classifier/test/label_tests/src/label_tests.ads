
with ML_Types;

package Label_Tests is

   procedure Test_Label_Encoder (Values  : ML_Types.Value_Data_List;
                                 Classes : ML_Types.Value_Data_List);
   procedure Test_Label_Encoder_Empty_Array
     (Values : ML_Types.Value_Data_List);
   procedure Test_Label_Encoder_Negative_Integers;

end Label_Tests;
