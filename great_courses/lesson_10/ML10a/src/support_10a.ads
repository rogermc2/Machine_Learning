
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_10A is

   type Data_Record (Num_Items, Num_Features : Positive) is record
      Features      : ML_Types.Value_Data_Array_2D (1 .. Num_Items,
                                                    1 .. Num_Features);
      Labels        : Integer_Array (1 .. Num_Items);
      Feature_Names : Unbounded_String_Array (1 .. Num_Features);
   end record;

   type Split_Data_Record
     (Num_Train, Num_Test, Num_Features : Positive) is record
      Train_Features : ML_Types.Value_Data_Array_2D (1 .. Num_Train,
                                                     1 .. Num_Features);
      Train_Labels   : Integer_Array (1 .. Num_Train);
      Test_Features  : ML_Types.Value_Data_Array_2D (1 .. Num_Test,
                                                     1 .. Num_Features);
      Test_Labels    : Integer_Array (1 .. Num_Test);
   end record;

   function Get_Data (File_Name : String) return Data_Record;
   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Matrix) return Float;
   pragma Inline (Error);
   function Split_Data (Data : Data_Record) return Split_Data_Record;

end Support_10A;
