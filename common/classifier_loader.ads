
with ML_Types;

package Classifier_Loader is

   type Feature_Type_Array is array (Positive range <>) of ML_Types.Data_Type;
   type Label_Type_Array is array (Positive range <>) of ML_Types.Data_Type;

   Value_Error : exception;

   function Load_Data (File_Name : String; Num_Outputs : Positive := 1;
                       Max_Lines : Positive := 20000)
                       return ML_Types.Multi_Output_Data_Record;

end Classifier_Loader;
