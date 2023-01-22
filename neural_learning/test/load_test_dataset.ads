
with NL_Types;
with ML_Types;

package Load_Test_Dataset is

   type Data_Record is record
      Features     : NL_Types.Float_List_2D;
      Target       : ML_Types.Integer_List;
      Num_Features : Positive;
   end record;

   function Load_Digits return Data_Record;
   function Load_Iris return Data_Record;

end Load_Test_Dataset;
