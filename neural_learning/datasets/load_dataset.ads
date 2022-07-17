
with NL_Types;

package Load_Dataset is

   type Digits_Data_Record is record
      Features     : NL_Types.Float_List_2D;
      Target       : NL_Types.Integer_List_2D;
      Num_Features : Positive;
   end record;

   type Iris_Data_Record is record
      Features     : NL_Types.Float_List_2D;
      Target       : NL_Types.Integer_List;
      Num_Features : Positive;

   end record;
   function Load_Digits (File_Name : String) return Digits_Data_Record;
   function Load_Iris (File_Name : String) return Iris_Data_Record;

end Load_Dataset;
